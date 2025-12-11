

for (hbs in unique(adult_inpatient_data$hb_name)){
  
plot_1 <- adult_inpatient_data |>
  filter(hb_name == hbs,
         FFN1 <= 1 | !is.na(FFN1)) |> 
  ggplot(aes(FFN1, sub_location_name, colour = sub_location_name)) +
  geom_boxplot(show.legend = F) 


plot_2 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           FFN2 <= 1 | !is.na(FFN2)) |> 
    ggplot(aes(FFN2, sub_location_name, colour = sub_location_name)) +
  geom_boxplot(show.legend = F) 
  
plot_3 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           FFN3 <= 1 | !is.na(FFN3)) |> 
    ggplot(aes(FFN3, sub_location_name, colour = sub_location_name)) +
  geom_boxplot(show.legend = F) 
  

plot_final <- plot_1 + plot_2 + plot_3 + patchwork::plot_layout(nrow = 1)
  
ggsave(filename = str_glue("Plots/FFN plots for {hbs}.png"), 
       plot = plot_final,
       width = 5000,
       height = 2000,
       units = "px")

}





for (hbs in unique(adult_inpatient_data$hb_name)){
  
  plot_1 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           AEW <= 1 | !is.na(AEW)) |> 
    ggplot(aes(AEW, sub_location_name, colour = sub_location_name)) +
    geom_boxplot(show.legend = F) 
  
  
  plot_2 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           EWF <= 1 | !is.na(EWF)) |> 
    ggplot(aes(EWF, sub_location_name, colour = sub_location_name)) +
    geom_boxplot(show.legend = F) 
  
  plot_final <- plot_1 + plot_2 + patchwork::plot_layout(nrow = 1)
  
  ggsave(filename = str_glue("Plots/AEW-EWF plots for {hbs}.png"), 
         plot = plot_final,
         width = 5000,
         height = 2000,
         units = "px")
  
}





for (hbs in unique(adult_inpatient_data$hb_name)){
  
  plot_1 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           !is.na(PUR)) |> 
    ggplot(aes(PUR, sub_location_name, colour = sub_location_name)) +
    geom_boxplot(show.legend = F) 
  
  
  plot_2 <- adult_inpatient_data |>
    filter(hb_name == hbs,
           !is.na(IFR)) |> 
    ggplot(aes(IFR, sub_location_name, colour = sub_location_name)) +
    geom_boxplot(show.legend = F) 
  
  plot_final <- plot_1 + plot_2 + patchwork::plot_layout(nrow = 1)
  
  ggsave(filename = str_glue("Plots/Falls-Ulcers plots for {hbs}.png"), 
         plot = plot_final,
         width = 5000,
         height = 2000,
         units = "px")
  
}


