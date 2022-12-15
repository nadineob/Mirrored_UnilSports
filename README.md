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

* `rvest`: for webscraping one webpage
* `rcrawler`: this package might also be used for webscraping across different webpages if needed
* `devtools`: for facilitating the package development process by providing R functions that simplify and expedite common tasks
* `lpSolve`: for the integer optimization to provide the daily exercise recommendation plan


## Data

We will use the data from Sports Universitaires Lausanne to create the main table containing the daily time table of sport activity.

* [Sports Universitaires Lausanne](https://sport.unil.ch/?mid=92)
* [MET Values for 800+ activities](https://golf.procon.org/met-values-for-800-activities/)

In addition, we will use the MET value data to create another table containing two columns; the list of sport activities (matching with the activities from Sports Universitaires Lausanne) and the number of calories burned based on the formula from [How to Calculate the Calories You Burn During Exercise](https://www.verywellfit.com/how-many-calories-you-burn-during-exercise-4111064). To calculate the number of calories burned for each user, we follow the below formula.

$$Total\ calories\ burned = Duration\ (minutes)\ *\ (MET\ *\ 3.5\ *weight\ in\ kg)/200 $$ 
Further information about MET can be found in [Using Metabolic Equivalent for Task (MET) for Exercises](https://www.verywellfit.com/met-the-standard-metabolic-equivalent-3120356). 

## UnilSports Shinny App

![](https://thumbs.gfycat.com/AcademicAssuredAvocet-size_restricted.gif){width="300" height="200}


### Optimization Function

Objective function: minimize the number of chosen activities
          Equation: min sum(x_i)
                    where x_i = 1 if the activity i is chosen and 0 if it's not chosen
Constraints:
1) The total calorie burn must exceed the target calorie
          Equation: sum(x_i*cal_i) >= calburn
                    where cal_i is the calorie burn of activity i and calburn is the target calorie

2) No overlapping time slots.
We set constraints such that the optimizer won't select 2 or more activities that occur at the same time
for example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15,
they cannot be selected together (i.e. only one of them can be selected)
          Equation: x_a + x_b + x_c +... <= 1 for all overlapping time intervals
                  if activity a, b, c,... have overlapping time slots

3) [Optional] Do not select the same activity
We set constraints such that the same activity cannot be selected.
for example, if there are several Football sessions, only 1 Football session can be selected.
          Equation: x_i + x_j + x_k + ... <= 1 for all duplicate activities
                  if activity i, j, k,... are the same activity

