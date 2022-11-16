
sim_t_test = function(true_mean) {
  
  sample = rnorm(30, mean = 0)
  
  test_results = t.test(sample)
  
  test_results %>% 
    broom::tidy()
}
# Need to wrap up these elements of the function into a mean!!!
# with the sim_t_test function, iterate across true mean values

df = expand_grid(
  true_mean = 0:6, 
  iterate = 1:5
) %>% 
  mutate(estimate_df = map(true_mean, sim_t_test))

homicide_summary %>% 
  filter(city_state == "Baltimore, MD") %>% 
  mutate(prop_test = map2(n_unsolved, n_total, ~ prop.test(.x, .y) %>% 
                            broom::tidy())) %>% unnest() %>% 
  select(estimate, conf.low, conf.high) %>% 
  mutate_all(~ . * 100) %>% 
  mutate(across(everything(), round, 2)) %>% 
  rename("Estimated proportion of unsolved homicides (%)" = estimate,
         "Lower confidence limit" = conf.low, 
         "Upper confidence limit" = conf.high) %>% 
  knitr::kable()