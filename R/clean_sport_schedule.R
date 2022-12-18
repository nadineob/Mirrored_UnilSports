#' Data of activity schedule in the UNIL sport center and the relevant The metabolic equivalent of task (MET) for each activity
#'
#' This table contains details of the sport activities in the UNIL sport center over 7 days, and also contains the metabolic equivalent of task (MET) of each activity as well as the number of calories burned per weights (kg) 
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{Date}{Date of each activity}
#'   \item{Start time}{The time at which activity begins} 
#'   \item{End time}{The time at which activity ends}
#'   \item{Duration_min}{The duration in minutes of activity}
#'   \item{Activity}{Activity name}
#'   \item{Location}{the location where activity takes place}
#'   \item{METs}{metabolic equivalent of task of each activity}
#'   \item{p}{calories burned per weight (in kilograms)}
#' }
#' @source \url{https://sport.unil.ch/, https://golf.procon.org/met-values-for-800-activities/}
"clean_sport_schedule"

