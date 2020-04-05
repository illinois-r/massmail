## ---- sample-graphic ----
theme_set(theme_bw())

ggplot(massmail_data_covid) +
  geom_bar(aes(`date`, fill = covid19)) +
  gghighlight(covid19, use_direct_label = FALSE) +
  labs(
    title = "Number of Times COVID-19 Showed up in Massmails Sent",
    y = "Frequency",
    x = "Date"
  ) + facet_wrap(~year(date))

ggplot(massmail_data_covid) +
  geom_bar(aes(weekday, fill = covid19)) +
  gghighlight(covid19, use_direct_label = FALSE) +
  labs(
    title = "Weekdays When Massmails Were Sent with COVID-19 Information",
    y = "Frequency",
    x = "Day of Week"
  )