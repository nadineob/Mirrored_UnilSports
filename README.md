# final-project-group_b
final-project-group_b created by GitHub Classroom
### Optimization 
  # Objective function: minimize the number of chosen activities
  #           Equation: min sum(x_i)
  #                     where x_i = 1 if the activity i is chosen and 0 if it's not chosen
  # Constraints:
  # 1) The total calorie burn must exceed the target calorie
  #           Equation: sum(x_i*cal_i) >= calburn
  #                     where cal_i is the calorie burn of activity i and calburn is the target calorie
  #
  # 2) No overlapping time slots. 
  # We set constraints such that the optimizer won't select 2 or more activities that occur at the same time
  # for example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15,
  # they cannot be selected together (i.e. only one of them can be selected) 
  #           Equation: x_a + x_b + x_c +... <= 1 for all overlapping time intervals
  #                   if activity a, b, c,... have overlapping time slots 
  # 
  # 3) [Optional] Do not select the same activity
  # We set constraints such that the same activity cannot be selected.
  # for example, if there are several Football sessions, only 1 Football session can be selected.
  #           Equation: x_i + x_j + x_k + ... <= 1 for all duplicate activities
  #                   if activity i, j, k,... are the same activity 
  #
