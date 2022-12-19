<!-- badges: start -->
[![R-CMD-check](https://github.com/nadineob/Mirrored_UnilSports/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nadineob/Mirrored_UnilSports/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
  
# UnilSports
final-project-group_b created by GitHub Classroom

The `UnilSports` makes it easier for UNIL/EPFL users or people interested in the activities of the University of Lausanne Sports Center to find their preferred sport more efficiently and to have an estimate of calorie burn.

## Motivation

The aim of this package is to optimize the selection of UNIL/EPFL sports center activities depending on the characteristics and needs of each individual. Given the huge variety that is offered, the activities are shortly defined and do not provide various information that people could be interested in such as the amount of calories that are burned, and the intensity of the activity. This information could be useful for those people who want to follow a more detailed track of their physical activity. Therefore, in this project we will be providing an interactive dashboard that suggests a personal timetable of different sports classes based on the user’s preference and availability which might facilitate the class-search process. This way it will be easier for UNIL/EPFL students to introduce the physical activity in their schedules and be motivated to do so.

## Disclaimer 

With this package the user will be able to visualize in a dashboard the daily exercise recommendation plan based on the user's preference. To obtain a recommendation, users will be required to fill in some information about themselves such as the following: 

* The number of calories that users would like to burn out per day.
* A preferable time slot that users would like to workout in. 
* Preferable sport activities. Similar to the preferable time slot, users can select multiple sport activities from a dropdown list of the classes provided.
* User's weight. This is needed to calculate the number of calories burned.

## Import Packages
In order to complete this project, we anticipate to use the following packages:

* `rvest` 
* `tidyverse` 
* `lubridate`
* `lpSolve`
* `dplyr`
* `data.table`
* `rlang`
* `plotly`
* `shiny`
* `shinythemes`
* `bslib`

## Data

We will use the data from Sports Universitaires Lausanne to create the main table containing the daily time table of sport activity.

* [Sports Universitaires Lausanne](https://sport.unil.ch/?mid=92)
* [MET Values for 800+ activities](https://golf.procon.org/met-values-for-800-activities/)

In addition, we will use the MET value data to create another table containing two columns; the list of sport activities (matching with the activities from Sports Universitaires Lausanne) and the number of calories burned based on the formula from [How to Calculate the Calories You Burn During Exercise](https://www.verywellfit.com/how-many-calories-you-burn-during-exercise-4111064). To calculate the number of calories burned for each user, we follow the below formula.

$$Total\ calories\ burned = Duration\ (minutes)\ *\ (MET\ *\ 3.5\ *weight\ in\ kg)/200 $$ 
Further information about MET can be found in [Using Metabolic Equivalent for Task (MET) for Exercises](https://www.verywellfit.com/met-the-standard-metabolic-equivalent-3120356). 

## UnilSports Shinny App

![](https://thumbs.gfycat.com/AcademicAssuredAvocet-size_restricted.gif)


## Optimization Function

Objective function: minimize the number of chosen activities
$$min\sum_{i=1}^N x_i$$

where $x_i$ is a binary variable that is 1 if activity $i$ is chosen and 0 if it is not chosen.

Constraints:

1) The total calorie burn must exceed the target calorie:
$$\sum_{i=1}^N (x_i \cdot cal_i) \geq calburn$$
where $cal_i$ is the calorie burn of activity $i$ and $calburn$ is the target calorie.

2) No overlapping time slots:
We set constraints to ensure that no more than one activity can be selected for any overlapping time interval.
For example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15,
they cannot be selected together (i.e. only one of them can be selected).
$$\sum_{i \in A} x_i + \sum_{i \in B} x_i + \sum_{i \in C} x_i \leq 1$$
for all overlapping time intervals where activity $A$, $B$, $C$, ... have overlapping time slots.

3) [Optional] Prohibiting the selection of duplicate activities:
We set constraints such that the same activity cannot be selected.
For example, if there are several Football sessions, only 1 Football session can be selected.
$$\sum_{i \in I} x_i + \sum_{i \in J} x_i + \sum_{i \in K} x_i + ... \leq 1$$
for all duplicate activities where activity $I$, $J$, $K$, ... are the same activity.

If the optimization algorithm is unable to find a solution, it indicates that there is no combination of activities whose total calorie expenditure exceeds the target calorie burn rate. In such case, the optimizer returns a combination of activities that results in the highest total calorie expenditure, rather than the minimum number of activities. This can be achieved by modifying the objective function and the first constraint as follows:

Modified objective function: maximize the total calorie burn
$$\max \sum_{i=1}^N (x_i \cdot cal_i)$$

where $x_i$ is a binary variable that is 1 if activity $i$ is chosen and 0 if it is not chosen, and $cal_i$ is the calorie burn of activity $i$.

Modified first constraint:
$$\sum_{i=1}^N (x_i \cdot cal_i) \leq calburn$$
