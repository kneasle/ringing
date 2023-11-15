use bellframe::{Mask, PlaceNot, RowBuf, Stage, Stroke};
use itertools::Itertools;
use monument::{
    parameters::{Call, CallDisplayStyle, CallId, Method, OptionalRangeInclusive, SpliceStyle},
    PartHeadGroup,
};

use crate::utils::len_range;

#[derive(Debug, Clone)]
pub struct Parameters {
    pub inner: monument::parameters::Parameters,
    pub maybe_unused_methods: Vec<(bool, Method)>,
    pub maybe_unused_calls: Vec<(bool, Call)>,
}

impl Parameters {
    pub fn to_monument(&self) -> monument::Parameters {
        let mut params = self.inner.clone();
        params.methods = self.used_methods().cloned().collect();
        params.calls = self.used_calls().cloned().collect();
        params
    }

    pub fn used_methods(&self) -> impl Iterator<Item = &Method> {
        self.maybe_unused_methods
            .iter()
            .filter(|(used, _m)| *used)
            .map(|(_, m)| m)
    }

    pub fn used_calls(&self) -> impl Iterator<Item = &Call> {
        self.maybe_unused_calls
            .iter()
            .filter(|(used, _c)| *used)
            .map(|(_, c)| c)
    }

    pub fn is_spliced(&self) -> bool {
        self.used_methods().count() > 1
    }

    pub fn yorkshire_s8_qps() -> Self {
        let stage = Stage::MAJOR;
        let cc_lib = bellframe::MethodLib::cc_lib().unwrap();

        let make_method = |title: &str, id: u16| {
            let mut method = cc_lib.get_by_title(title).unwrap();
            method.set_lead_end_label();
            monument::parameters::Method {
                id: monument::parameters::MethodId(id),

                custom_shorthand: String::new(),
                inner: method,
                count_range: OptionalRangeInclusive::OPEN,
                start_indices: vec![0],
                end_indices: (0..32).collect_vec(),
                allowed_courses: vec![Mask::parse_with_stage("1*", stage).unwrap().into()],
            }
        };

        // Calls
        let bob = Call::lead_end_call(CallId(0), PlaceNot::parse("14", stage).unwrap(), "-", -1.8);
        let single = Call::lead_end_call(
            CallId(1),
            PlaceNot::parse("1234", stage).unwrap(),
            "s",
            -2.5,
        );
        let maybe_unused_calls = vec![(true, bob), (true, single)];

        let monument_params = monument::Parameters {
            length: len_range(1250, 1350),
            stage,
            num_comps: 100,
            require_truth: true,

            methods: index_vec::index_vec![],
            splice_style: SpliceStyle::LeadLabels,
            splice_weight: 0.0,
            calls: index_vec::index_vec![],
            call_display_style: CallDisplayStyle::CallingPositions(stage.tenor()),
            atw_weight: None, // Don't calculate atw
            require_atw: false,

            start_row: RowBuf::rounds(stage),
            end_row: RowBuf::rounds(stage),
            part_head_group: PartHeadGroup::one_part(stage),
            course_weights: vec![],

            music_types: index_vec::index_vec![],
            start_stroke: Stroke::Hand,
        };

        crate::Parameters {
            inner: monument_params,
            maybe_unused_methods: vec![
                (true, make_method("Cambridge Surprise Major", 0)),
                (true, make_method("Yorkshire Surprise Major", 1)),
                (false, make_method("Superlative Surprise Major", 2)),
            ],
            maybe_unused_calls,
        }
    }
}
