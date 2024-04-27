# Parameter Quick Reference

This section provides an exhaustive list of Monument's parameters.

This is intended as a 'Quick Reference Guide' for existing users and assumes working knowledge of
Monument.  If you want to learn how Monument works, check out the
['Get Started' section](/get-started.md) of the docs.

## Quick Links

### [General Parameters](general.html)
- [`length`](general.html#length)
- [`num_comps`](general.html#num_comps)
- [`allow_false`](general.html#allow_false)
- [`queue_limit`](general.html#queue_limit)
- [`graph_size_limit`](general.html#graph_size_limit)

### [Defining Methods](method-general.html)
- [`splice_style`](method-general.html#splice_style)
- [`method_count`](method-general.html#method_count)
- [`splice_weight`](method-general.html#splice_weight)
- [`method` array:](#per-method.html)
  - [`title`](per-method.html#by-title) or
    [(`name`, `place_notation` and `stage`)](per-method.html#by-place-notation)
  - [`shorthand`](per-method.html#shorthand)
  - [`labels`](per-method.html#labels)
  - [`count`](per-method.html#count)
  - [`courses`](per-method.html#courses)
  - [`start_indices`](per-method.html#start_indices)
  - [`end_indices`](per-method.html#end_indices)

### [Courses](courses.html)
- [`part_head`](courses.html#part_head)
- [`courses`](courses.html#courses)
- [`split_tenors`](courses.html#split_tenors)
- [`course_weights`](courses.html#course_weights)
- [`handbell_coursing_weight`](courses.html#handbell_coursing_weight)
- ~~[`leadwise`](courses.html#leadwise)~~ (removed in v0.10.0)

### [Music](music.html)
- [`base_music`](music.html#base_music)
- [`music_file`](music.html#music_file)
- [`music`](music.html#music) array:
  - [`weight`](music.html#weight)
  - [`count`](music.html#count)
  - [`stroke`](music.html#stroke)
  - [`show` and `name`](music.html#show-and-name)

### [Defining Calls](calls.html)
- [`base_calls`](calls.html#base_calls)
- [`bobs_only` and `singles_only`](calls.html#bobs_only-and-singles_only)
- [`bob_weight` and `single_weight`](calls.html#bob_weight-and-single_weight)
- [`calls` array:](calls.html#calls)
  - [`place_notation`](calls.html#place_notation)
  - [`symbol`](calls.html#symbol)
  - [`label`](calls.html#label)
  - [`weight`](calls.html#weight)
  - [`calling_positions`](calls.html#calling_positions)

### [Starts-ends](starts-ends.html)
- [`snap_start`](starts-ends.html#snap_start)
- [`start_indices` and `end_indices`](starts-ends.html#start_indices-and-end_indices)
- [`start_stroke`](starts-ends.html#start_stroke)

### [Non-Duffers](non-duffers.html)
- [`non_duffer_courses`](non-duffers.html#non_duffer_courses)
- [`max_total_duffer`](non-duffers.html#max_total_duffer)
- [`max_consecutive_duffer`](non-duffers.html#max_consecutive_duffer)

