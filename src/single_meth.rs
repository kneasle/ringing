use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter},
    ops::Range,
};

use itertools::Itertools;
use proj_core::{
    place_not::PnBlockParseError, Bell, Method, PlaceNot, PnBlock, Row, RowTrait, Stage,
};

use crate::{
    engine::{self, CompRow, Node},
    music::{MusicPattern, MusicTable},
    spec::CallSpec,
};

/// A tuple of values which represent the transition between two segments
type Transition = (Call, Row, usize);

/// A compact representation of a call
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Call {
    call: Option<char>,
    position: char,
}

impl Call {
    fn new(call: Option<char>, position: char) -> Self {
        Call { call, position }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.call.unwrap_or('p'), self.position)
    }
}

/// A section of a course of a single method
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Section {
    ind: usize,
}

impl Section {
    const fn new(ind: usize) -> Self {
        Section { ind }
    }
}

impl Display for Section {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ind)
    }
}

impl From<Section> for usize {
    #[inline(always)]
    fn from(s: Section) -> usize {
        s.ind
    }
}

/// The persistent state table for a single method
#[derive(Debug, Clone)]
pub struct Table<R: CompRow> {
    falseness: Vec<Vec<(R, Section)>>,
    next_nodes: Vec<Vec<(Call, R, Section)>>,
    music_tables: Vec<MusicTable>,
    lengths: Vec<usize>,
    stage: Stage,
}

impl Table<Row> {
    pub fn from_place_not(
        stage: Stage,
        method_pn: &str,
        fixed_bell_chars: &str,
        calls: &[CallSpec],
        plain_lead_calling_positions: &str,
    ) -> Result<Self, PnBlockParseError> {
        let (method, fixed_bells) = Self::parse_pn(stage, method_pn, fixed_bell_chars)?;

        Ok(Self::from_method(
            &method,
            fixed_bells,
            calls,
            plain_lead_calling_positions,
        ))
    }

    fn parse_pn(
        stage: Stage,
        method_pn: &str,
        fixed_bell_chars: &str,
    ) -> Result<(Method, Vec<Bell>), PnBlockParseError> {
        let method = Method::with_lead_end(String::new(), &PnBlock::parse(method_pn, stage)?);
        let fixed_bells = fixed_bell_chars
            .chars()
            .map(|c| Bell::from_name(c).unwrap())
            .collect_vec();
        Ok((method, fixed_bells))
    }

    /// Generates a new table for a known method
    pub fn from_method(
        method: &Method,
        fixed_bells: Vec<Bell>,
        calls: &[CallSpec],
        plain_lead_calling_positions: &str,
    ) -> Self {
        let stage = method.stage();
        let (plain_course, fixed_bells, ranges, transitions) =
            Self::generate_ranges(method, fixed_bells, calls, plain_lead_calling_positions);

        Self::new(
            stage,
            plain_course,
            &fixed_bells,
            &ranges,
            transitions,
            MusicPattern::runs_front_or_back(stage, 4, 1.0),
        )
    }

    /// A helper function to generate the ranges & transitions for a given method and calls.  This
    /// is made into a helper function so it can be easily tested in isolation.
    #[allow(clippy::type_complexity)]
    fn generate_ranges(
        method: &Method,
        fixed_bells: Vec<Bell>,
        call_specs: &[CallSpec],
        plain_lead_calling_positions: &str,
    ) -> (
        // The method's plain course
        Vec<Row>,
        // The parsed fixed bells
        Vec<Bell>,
        // The ranges of the course
        Vec<Range<usize>>,
        // The transitions between the ranges
        Vec<Vec<Transition>>,
    ) {
        /* Parse everything and cache commonly-used values */

        let stage = method.stage();
        let calls = call_specs
            .iter()
            .map(
                |CallSpec {
                     place_not_str,
                     symbol,
                     calling_positions,
                 }| {
                    (
                        PlaceNot::parse(&place_not_str, stage).unwrap(),
                        symbol,
                        calling_positions,
                    )
                },
            )
            .collect_vec();

        let tenor = Bell::tenor(stage).unwrap();
        let plain_course = method.plain_course();
        let lead_len = method.lead_len();
        let course_len = plain_course.len();
        let lead_end_indices = plain_course
            .annots()
            .enumerate()
            .filter_map(|(i, (_, pos))| {
                pos.filter(|s| s == &proj_core::method::LABEL_LEAD_END)
                    .map(|_| i)
            })
            .collect_vec();

        /* Generate a mapping from fixed bell indices to the lead head and its index, which we'll
         * then use when deciding which calling positions are and aren't allowed. */

        let pc_lead_heads: HashMap<Vec<usize>, (usize, &Row)> = plain_course.annot_rows()
            [..course_len - 1]
            .iter()
            .enumerate()
            .step_by(lead_len)
            .map(|(i, r)| (get_bell_inds(&fixed_bells, r.row()), (i, r.row())))
            .collect();

        /* Generate a map of which calls preserve fixed bells, and what course jump occurs as a
         * result of those calls. */

        let mut call_jumps = Vec::<(usize, usize, Row, Call)>::new();
        for (pn, call_name, calling_positions) in &calls {
            // Test this call at every lead end and check that it keeps the fixed bells in plain
            // coursing order
            for &lead_end_ind in &lead_end_indices {
                // This unsafety is OK because all the rows & pns are parsed within this function,
                // which is provided a single Stage
                let new_lh = unsafe {
                    pn.permute_new_unchecked(plain_course.get_row(lead_end_ind).unwrap())
                };
                // Check that this call hasn't permuted the fixed bells
                if let Some(&(lh_ind, pc_lh)) =
                    pc_lead_heads.get(&get_bell_inds(&fixed_bells, &new_lh))
                {
                    let tenor_place = new_lh.place_of(tenor).unwrap();
                    let call_pos = calling_positions
                        .get(tenor_place)
                        .expect("Calling positions are too short");
                    // This unsafety is OK because all the rows & pns are parsed within this
                    // function, which is provided a single Stage
                    let new_course_head = unsafe { pc_lh.tranposition_to_unchecked(&new_lh) };
                    call_jumps.push((
                        lead_end_ind,
                        lh_ind,
                        new_course_head,
                        Call::new(Some(**call_name), *call_pos),
                    ));
                }
            }
        }

        /* Use this call mapping to split every course into a set of (not necessarily mutually
         * exclusive) ranges */

        // We have to start a range after every call, and end a range just before that call
        let mut range_starts = call_jumps.iter().map(|(_, to, ..)| *to).collect_vec();
        let mut range_ends = call_jumps.iter().map(|(from, ..)| *from).collect_vec();
        // If a call is omitted, then we can perform an un-jump from one lead end to the
        // consecutive lead head.  These have to be added manually if we have calls which affect
        // but don't permute fix bells (e.g. 4ths place calls in n-ths place methods).
        range_starts.extend(range_ends.iter().map(|&x| (x + 1) % course_len));

        // Sort and de-duplicate the starts and ends (later algorithms rely on binary searching
        // these arrays)
        range_starts.sort_unstable();
        range_starts.dedup();
        range_ends.sort_unstable();
        range_ends.dedup();

        // Calculate the ranges.  Each of `range_starts` corresponds to a unique range, which ends
        // at the first range_end which is encountered (wrapping round the end of the course if
        // needed).  **NOTE:** this also increments all end-points, so that the ranges are
        // half-open (which is the semantics of `..`)
        let mut ranges = range_starts
            .iter()
            .map(|&start| {
                let range_end_ind = range_ends.binary_search(&start).unwrap_err();
                let end = *range_ends.get(range_end_ind).unwrap_or(&range_ends[0]);
                // `end` is an inclusive bound, but `..` is half-open.  Therefore, we add one here
                // to correct for this
                start..end + 1
            })
            .collect_vec();

        // Rotate the sections so that the start of the course appears in section #0 (e.g. if we're
        // using far calls in an nths place method).
        if ranges[0].start != 0 {
            let r = ranges.pop().unwrap();
            ranges.insert(0, r);
        }

        /* Use the parsed call data to generate which ranges can be joined together. */

        // Conversion table from range starts to their index within `ranges`
        let range_index_by_start = ranges
            .iter()
            .enumerate()
            .map(|(i, r)| (r.start, i))
            .collect::<HashMap<_, _>>();

        let transitions = ranges
            .iter()
            .map(|range| {
                // For fragments that end at the course head, `range.end` will equal `course_len`
                // but the modulo maps that back to 0 so it can be connected to the first segment
                let lead_head_index = range.end % course_len;
                let plain_lead_tenor_place = plain_course
                    .get_row(lead_head_index)
                    .unwrap()
                    .place_of(tenor)
                    .unwrap();
                let plain_calling_pos = plain_lead_calling_positions
                    .chars()
                    .nth(plain_lead_tenor_place)
                    .expect("Plain calling position string is shorter than stage");

                // Each call which starts at the last row of this range could cause a call
                let mut ts = vec![(
                    Call::new(None, plain_calling_pos),
                    Row::rounds(stage),
                    range_index_by_start[&lead_head_index],
                )];
                ts.extend(
                    call_jumps
                        .iter()
                        // Remove any calls that don't appear at the end of this range.  The `- 1`
                        // corrects for the fact that call starts refer to lead ends, whereas
                        // ranges are half open and therefore refer to the lead **head**.  We also
                        // use modulo to make the subtraction wrap around the end of the course
                        // instead of overflowing.
                        .filter(|(from, ..)| *from == (range.end + course_len - 1) % course_len)
                        .map(|(_from, to, course_head, name)| {
                            (*name, course_head.clone(), range_index_by_start[to])
                        }),
                );
                ts
            })
            .collect_vec();

        let mut rows = plain_course.rows().cloned().collect_vec();
        // `rows` starts and ends with rounds, so we pop the rounds at the end since it isn't
        // needed
        assert!(rows.pop().unwrap().is_rounds());

        (rows, fixed_bells, ranges, transitions)
    }

    pub fn new(
        stage: Stage,
        // The rows of the plain course, NOT including the finishing rounds
        plain_course_rows: Vec<Row>,
        fixed_bells: &[Bell],
        ranges: &[Range<usize>],
        transitions: Vec<Vec<Transition>>,
        music_patterns: Vec<MusicPattern>,
    ) -> Self {
        /* Group rows in each range by the locations of the fixed bells.  By the definition of
         * fixed bells, we only consider falseness between rows which have the fixed bells in the
         * same places. */

        // Calculate the correct range lengths (handling ranges which wrap round the end of the
        // course)
        let plain_course_len = plain_course_rows.len();
        let range_lengths = ranges
            .iter()
            // We can't simply call `r.len()`, because we have to handle ranges which wrap over the
            // end of the course.  This will make `r.start <= r.end` resulting in the block
            // erroneously being given length 0.  We also add `plain_course_len` before the
            // subtraction in order to prevent overflowing past 0
            .map(|r| (r.end + plain_course_len - r.start) % plain_course_len)
            .collect_vec();

        type FalseMap<'a> = HashMap<Vec<usize>, Vec<&'a Row>>;

        let grouped_rows: Vec<FalseMap> = ranges
            .iter()
            .zip(range_lengths.iter())
            .map(|(range, len)| {
                // Group all the rows by the indices of the fixed bells
                let mut rows_by_fixed_bell_indices: FalseMap = HashMap::with_capacity(*len);

                // Correctly handle ranges which wrap round the end of a course
                let ranges = if range.start < range.end {
                    vec![range.clone()]
                } else {
                    // TODO: Stop the off-by-one indexing errors
                    vec![range.start..plain_course_len, 0..range.end]
                };

                for range in ranges {
                    for r in &plain_course_rows[range.clone()] {
                        let fixed_bell_inds = get_bell_inds(fixed_bells, r);
                        rows_by_fixed_bell_indices
                            .entry(fixed_bell_inds)
                            .or_insert_with(Vec::new)
                            .push(r);
                    }
                }
                // Return this grouping so it can be combined to generate the falseness table
                rows_by_fixed_bell_indices
            })
            .collect();

        /* Use these grouped rows to iterate over all pairs of ranges and use this to generate a
         * map of ranges are false against which ranges of the plain course. */

        // If (i, j, r) is in this set, then it means that range `i` of the plain course is false
        // against range `j` of the course starting with `r`.
        //
        // Equivalently, if (i, j, r) is in this set then it means that the range `i` of some course
        // `s` is false against the range `j` of the course starting with `s * r`.
        let mut falseness_map: HashSet<(usize, usize, Row)> = HashSet::new();
        // Iterate over every pair of ranges to compute the relative falseness
        for ((i1, map1), (i2, map2)) in grouped_rows
            .iter()
            .enumerate()
            .cartesian_product(grouped_rows.iter().enumerate())
        {
            // If `map1` and `map2` contain entries with the same locations of the fixed bells,
            // then this will cause some transposition of them to be false
            for (fixed_bell_inds, rows1) in map1.iter() {
                if let Some(rows2) = map2.get(fixed_bell_inds) {
                    for (&r1, &r2) in rows1.iter().cartesian_product(rows2.iter()) {
                        // If
                        //      `r1` from range `i1`
                        //    has the same pattern of fixed bells as
                        //      `r2` from range `i2`,
                        // then
                        //      the range `i1` of the plain course
                        //    is false against
                        //      the range `i2` of `r2.tranposition_to(r1)`
                        //
                        //  (i.e. we find `X` where `X * r2 == r1`)
                        let false_course = unsafe { r2.tranposition_to_unchecked(r1) };
                        falseness_map.insert((i1, i2, false_course));
                    }
                }
            }
        }

        // Convert the hash table into a jagged 2D array, indexed by the first element of the tuple
        // (so that the lookups we want to do are faster).
        let mut final_table: Vec<Vec<(Row, Section)>> = vec![Vec::new(); ranges.len()];
        for (i, j, r) in falseness_map {
            final_table[i].push((r, Section::new(j)));
        }

        // Sort the falseness tables (which will give better cache performance and is also easier
        // just to read)
        final_table
            .iter_mut()
            .for_each(|v| v.sort_by(|a, b| (a.1.ind, &a.0).cmp(&(b.1.ind, &b.0))));

        // Generate the music tables for each section
        let music_tables = ranges
            .iter()
            .map(|r| {
                let music_table = if r.start < r.end {
                    MusicTable::from_rows(
                        stage,
                        fixed_bells,
                        &plain_course_rows[r.clone()],
                        &music_patterns,
                    )
                } else {
                    MusicTable::from_rows(
                        stage,
                        fixed_bells,
                        plain_course_rows[r.start..]
                            .iter()
                            .chain(plain_course_rows[..r.end].iter()),
                        &music_patterns,
                    )
                };
                music_table
            })
            .collect_vec();

        Table {
            falseness: final_table,
            lengths: range_lengths,
            stage,
            music_tables,
            next_nodes: transitions
                .into_iter()
                .map(|vs| {
                    vs.into_iter()
                        .map(|(s, r, i)| (s, r, Section::new(i)))
                        .collect()
                })
                .collect(),
        }
    }

    #[allow(dead_code)]
    pub fn print_falseness(&self) {
        for (i, secs) in self.falseness.iter().enumerate() {
            println!("{}", i);
            for (r, sec) in secs {
                println!("   {}: {}", sec.ind, r);
            }
        }
    }

    pub fn change_row_type<R: CompRow + From<Row>>(self) -> Table<R> {
        Table {
            falseness: self
                .falseness
                .into_iter()
                .map(|rs| rs.into_iter().map(|(r, s)| (R::from(r), s)).collect_vec())
                .collect_vec(),
            next_nodes: self
                .next_nodes
                .into_iter()
                .map(|vs| {
                    vs.into_iter()
                        .map(|(c, r, s)| (c, R::from(r), s))
                        .collect_vec()
                })
                .collect_vec(),
            stage: self.stage,
            lengths: self.lengths,
            music_tables: self.music_tables,
        }
    }
}

impl<R: CompRow> engine::Table<R> for Table<R> {
    type Section = Section;
    type Call = Call;

    #[inline(always)]
    fn stage(&self) -> Stage {
        self.stage
    }

    #[inline(always)]
    fn start() -> Self::Section {
        Section::new(0)
    }

    #[inline(always)]
    fn num_sections(&self) -> usize {
        self.lengths.len()
    }

    #[inline(always)]
    fn is_end(node: &Node<R, Self::Section>) -> bool {
        node.row.is_rounds() && node.section.ind == 0
    }

    #[inline(always)]
    fn length(&self, section: Self::Section) -> usize {
        self.lengths[section.ind]
    }

    #[inline(always)]
    fn falseness(&self, section: Self::Section) -> &[(R, Self::Section)] {
        self.falseness[section.ind].as_slice()
    }

    #[inline(always)]
    fn expand(&self, section: Self::Section) -> &[(Self::Call, R, Self::Section)] {
        self.next_nodes[section.ind].as_slice()
    }

    #[inline(always)]
    fn music(&self, node: &Node<R, Self::Section>) -> f32 {
        self.music_tables[node.section.ind].evaluate(&node.row)
    }

    fn comp_string(&self, calls: &[Self::Call]) -> String {
        calls
            .iter()
            .map(|Call { call, position }| match call {
                // Plain leads don't get displayed
                None => String::new(),
                // Bobs are implicit
                Some('-') => format!("{}", position),
                // Any other call is written out in full
                Some(name) => format!("{}{}", name, position),
            })
            .join("")
    }
}

/// Returns the indices of a set of [`Bells`] within a given [`Row`]
fn get_bell_inds(bells: &[Bell], r: &Row) -> Vec<usize> {
    bells.iter().map(|b| r.place_of(*b).unwrap()).collect_vec()
}

/// Returns the list of fixed [`Bell`]s which only allows 2-6 to be affected
pub fn tenors_together_fixed_bells(stage: Stage) -> Vec<Bell> {
    let mut fixed_bells = vec![Bell::TREBLE];
    fixed_bells.extend((6..stage.as_usize()).map(Bell::from_index));
    fixed_bells
}

#[cfg(test)]
mod tests {
    use super::*;

    struct RangeGenInput<'a> {
        stage: Stage,
        method_pn: &'a str,
        fixed_bell_chars: &'a str,
        calls: Vec<CallSpec>,
        plain_lead_calling_positions: &'a str,
    }

    impl<'a> RangeGenInput<'a> {
        fn new(
            stage: Stage,
            method_pn: &'a str,
            fixed_bell_chars: &'a str,
            calls: Vec<CallSpec>,
            plain_lead_calling_positions: &'a str,
        ) -> Self {
            Self {
                stage,
                method_pn,
                fixed_bell_chars,
                calls,
                plain_lead_calling_positions,
            }
        }
    }

    /// Helper function to test a single method's table generation (used to reduce the amount of
    /// code used for the test specifications).
    fn test_range_gen(
        input: RangeGenInput<'_>,
        exp_ranges: &[Range<usize>],
        exp_transitions: &[Vec<(Option<char>, char, &str, usize)>],
    ) {
        // Parse the transitions into the expected format
        let parsed_transitions: Vec<Vec<Transition>> = exp_transitions
            .iter()
            .map(|xs| {
                xs.iter()
                    .map(|(c_name, c_pos, ch, next_seg)| {
                        (
                            Call::new(*c_name, *c_pos),
                            Row::parse_with_stage(ch, input.stage).unwrap(),
                            *next_seg,
                        )
                    })
                    .collect_vec()
            })
            .collect_vec();

        // Generate the regions
        let (method, fixed_bells) =
            Table::parse_pn(input.stage, input.method_pn, input.fixed_bell_chars).unwrap();
        let (_pc, _fixed_bells, ranges, transitions) = Table::generate_ranges(
            &method,
            fixed_bells,
            &input.calls,
            input.plain_lead_calling_positions,
        );

        // Test the output
        assert_eq!(ranges, exp_ranges);
        assert_eq!(transitions, parsed_transitions);
    }

    #[test]
    fn region_gen_cambs_8() {
        test_range_gen(
            // "-38-14-1256-18-12-58-16-78,12", // Cooktown
            RangeGenInput::new(
                Stage::MAJOR,
                "-38-14-1258-36-14-58-16-78,12",
                "178",
                CallSpec::near(Stage::MAJOR),
                "LBTFVMWH",
            ),
            &[0..64, 64..96, 96..128, 128..224, 160..224],
            &[
                vec![(None, 'B', "12345678", 1), (Some('-'), 'B', "13526478", 4)],
                vec![
                    (None, 'M', "12345678", 2),
                    (Some('-'), 'M', "14365278", 2),
                    (Some('s'), 'M', "16345278", 2),
                ],
                vec![
                    (None, 'W', "12345678", 3),
                    (Some('-'), 'W', "15243678", 3),
                    (Some('s'), 'W', "15342678", 3),
                ],
                vec![
                    (None, 'H', "12345678", 0),
                    (Some('-'), 'H', "14235678", 0),
                    (Some('s'), 'H', "12435678", 0),
                ],
                vec![
                    (None, 'H', "12345678", 0),
                    (Some('-'), 'H', "14235678", 0),
                    (Some('s'), 'H', "12435678", 0),
                ],
            ],
        );
    }

    #[test]
    fn region_gen_bristol_8() {
        test_range_gen(
            RangeGenInput::new(
                Stage::MAJOR,
                "-58-14.58-58.36.14-14.58-14-18,18",
                "178",
                CallSpec::near(Stage::MAJOR),
                "LIBMFHVW",
            ),
            &[0..32, 32..64, 64..128, 128..224, 192..224],
            &[
                vec![
                    (None, 'H', "12345678", 1),
                    (Some('-'), 'H', "14235678", 0),
                    (Some('s'), 'H', "12435678", 0),
                ],
                vec![
                    (None, 'M', "12345678", 2),
                    (Some('-'), 'M', "14365278", 1),
                    (Some('s'), 'M', "16345278", 1),
                ],
                vec![(None, 'B', "12345678", 3), (Some('-'), 'B', "13526478", 3)],
                vec![
                    (None, 'W', "12345678", 0),
                    (Some('-'), 'W', "15243678", 4),
                    (Some('s'), 'W', "15342678", 4),
                ],
                vec![
                    (None, 'W', "12345678", 0),
                    (Some('-'), 'W', "15243678", 4),
                    (Some('s'), 'W', "15342678", 4),
                ],
            ],
        );
    }

    #[test]
    fn region_gen_bristol_12() {
        test_range_gen(
            RangeGenInput::new(
                Stage::MAXIMUS,
                "-5T-14.5T-5T.36.14-7T.58.16-9T.70.18-18.9T-18-1T,1T",
                "17890ET",
                CallSpec::near(Stage::MAXIMUS),
                "LIB?F??M?HVW",
            ),
            &[0..144, 144..336, 192..336, 336..528],
            &[
                vec![
                    (None, 'M', "1234567890ET", 1),
                    (Some('-'), 'M', "1436527890ET", 3),
                    (Some('s'), 'M', "1634527890ET", 3),
                ],
                vec![
                    (None, 'H', "1234567890ET", 3),
                    (Some('-'), 'H', "1423567890ET", 0),
                    (Some('s'), 'H', "1243567890ET", 0),
                ],
                vec![
                    (None, 'H', "1234567890ET", 3),
                    (Some('-'), 'H', "1423567890ET", 0),
                    (Some('s'), 'H', "1243567890ET", 0),
                ],
                vec![
                    (None, 'W', "1234567890ET", 0),
                    (Some('-'), 'W', "1524367890ET", 2),
                    (Some('s'), 'W', "1534267890ET", 2),
                ],
            ],
        );
    }

    #[test]
    fn region_gen_bristol_royal_far_calls() {
        test_range_gen(
            RangeGenInput::new(
                Stage::ROYAL,
                "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
                "17890",
                CallSpec::far(Stage::ROYAL),
                "LIO?VM?HVW",
            ),
            &[200..120, 120..160, 160..200],
            &[
                vec![
                    (None, 'V', "1234567890", 1),
                    (Some('-'), 'V', "1632547890", 1),
                    (Some('s'), 'V', "1634527890", 1),
                ],
                vec![
                    (None, 'O', "1234567890", 2),
                    (Some('-'), 'O', "1342567890", 2),
                    (Some('s'), 'O', "1243567890", 2),
                ],
                vec![
                    (None, 'I', "1234567890", 0),
                    (Some('-'), 'I', "1354267890", 0),
                    (Some('s'), 'I', "1534267890", 0),
                ],
            ],
        );
    }
}
