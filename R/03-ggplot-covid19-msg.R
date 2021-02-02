## ---- sample-graphic ----
theme_set(theme_bw())

ggplot(massmail_data_covid) +
  geom_bar(aes(`date`, fill = covid19)) +
  gghighlight(covid19, use_direct_label = FALSE, calculate_per_facet = TRUE) +
  labs(
    title = "Number of Times COVID-19 Showed up in Massmails Sent",
    y = "Frequency",
    x = "Date"
  ) + 
  facet_wrap(~year(date), scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

ggplot(massmail_data_covid) +
  geom_bar(aes(weekday, fill = covid19)) +
  gghighlight(covid19, use_direct_label = FALSE, calculate_per_facet = TRUE) +
  labs(
    title = "Weekdays When Massmails Were Sent with COVID-19 Information",
    y = "Frequency",
    x = "Day of Week"
  ) + 
  facet_wrap(~year(date))
