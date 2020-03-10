#' NBA Season Stats
#'
#' This function allows you to input the year
#' @param year What year? Defaults to TRUE.
#' @keywords ``age, team, continuous
#' @export
#' @examples
#' fun1(), fun2(), fun3()

# --- FUNCTION 1 ---
# input - individual year
# output - data point of interest in the dataset for some player
fun1 <- function(y){
  f1 <- df %>%
    filter(Year == y) %>%
    select(Year, Tm) %>%
    group_by(Tm)

  return(head(f1, 10))
}


# --- FUNCTION 2 ---
# input - individual year
# output - oldest player

fun2 <- function(y){
  old_player <- df %>%
    select(Year, Age) %>%
    filter(Year == y) %>%
    group_by(Age) %>%
    arrange(desc(Age))

  return(head(old_player, 10))
}


# --- FUNCTION 3 ---
# input - individual year
# ouput - continuous variables only

fun3 <- function(y) {
  df3 <- keep(df, is.integer)
  df3 <- df3 %>% filter(df3$Year == y)
  return(cor(df3))
}
