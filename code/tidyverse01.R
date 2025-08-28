
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)
filter(iris_sub, Species == "virginica")
filter(iris_sub, Species %in% c("virginica", "versicolor"))
filter(iris_sub, Species != "virginica")
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))
filter(iris_sub, Sepal.Length > 5)
filter(iris_sub, Sepal.Length >= 5)
filter(iris_sub, Sepal.Length < 5)
filter(iris_sub, Sepal.Length <= 5)
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")
filter(iris_sub,
       Sepal.Length < 5 | Species == "setosa")
arrange(iris_sub, Sepal.Length)
arrange(iris_sub, desc(Sepal.Length))
select(iris_sub, c(Sepal.Length, Sepal.Width))
select(iris_sub, Sepal.Length)
select(iris_sub, -Sepal.Length)
select(iris_sub, -c(Sepal.Length, Sepal.Width))
select(iris_sub, starts_with("Sepal"))
select(iris_sub, -starts_with("Sepal"))
select(iris_sub, ends_with("Width"))
select(iris_sub, -ends_with("Width"))
(x_max <- nrow(iris_sub))
x <- 1 : x_max
mutate(iris_sub, row_id = x)
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

print(df_vir_sl)
df_vir_sl <- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

print(df_vir_sl)

iris_setosa <- filter(iris_sub, Species == "setosa")
iris_num <- select(iris_setosa, -Species)
iris_num <- iris_sub %>% 
  filter(Species == "setosa") %>% 
  select(-Species)

# exercise ----------------------------------------------------------------

filter(iris_sub, Species == "virginica")

select(iris_sub, Sepal.Width)

df0 <- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Width) %>% 
  mutate (sw3 = 3 * Sepal.Width)

mean(c(2, 5, 8))
sum(c(2, 5, 8))
m_large <- mean(1:1000)
s_sum <- sum(1:1000)

mean(iris_sub$Sepal.Length)

iris_sub %>% 
  group_by(Species) %>% 
  summarize(mean_sl = mean(Sepal.Length),
            sum_sl = sum(Sepal.Length),
            mean_pl = mean(Petal.Length),
            sum_pl = sum(Petal.Length))
## join
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))
df2 <- tibble(Species = c("A", "B", "C"),
              y = c(10, 12, 13))
left_join(x = df1,
          y = df2,
          by = "Species")


df2_minus_c <- tibble(Species = c("A", "B"),
                      y = c(10, 12))

left_join(x = df1,
          y = df2_minus_c,
          by = "Species")

          
