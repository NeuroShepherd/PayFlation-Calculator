

plot <- calculator_net_inflation_table_cleaned_default %>%
  arrange(Year) %>%
  mutate(`Cumulative Inflation` = cumprod(`Effective Inflation`),
         `Annual Inflation Percent` = `Annual Inflation`*100) %>%
  ggplot(.,aes(x=Year)) +
  theme_bw() +
  geom_line(aes(y=`Cumulative Inflation`), size=2, color="red") +
  # geom_line(aes(y=log(`Cumulative Inflation`)), size=2, color="red") +
  # no real reason to include log(inflation), but made the code for fun. unlikely to include
  geom_bar(aes(y=`Annual Inflation Percent`), stat = "identity", fill = "#2c6dd4")


(plot2 <- plot +
  scale_y_continuous(labels = scales::label_percent(big.mark = ","),
                     sec.axis = sec_axis(trans = ~./100, name = "Yearly Inflation %",
                                         labels = scales::percent)) +
  theme(axis.title.y.left = element_text(color = "red"),
        axis.text.y.left = element_text(color = "red"),
        axis.title.y.right = element_text(color = "#2c6dd4"),
        axis.text.y.right = element_text(color = "#2c6dd4"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(1910,2025, 5))
)


  plotly::ggplotly(plot2, tooltip = "text")
    layout(yaxis2 = list(overlaying = "y", side = "right"))
