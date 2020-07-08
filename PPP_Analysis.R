library(tidyverse)
dev.off()
#Read in Files
PPP <- read.csv("/Users/samedelstein/Downloads/150k\ plus/PPP\ Data\ 150k\ plus.csv")
#Define Zip Codes in Onondaga County
zip_codes <- c('13135',
                                 '13137',
                                 '13138',
                                 '13141',
                                 '13152',
                                 '13153',
                                 '13159',
                                 '13164',
                                 '13202',
                                 '13203',
                                 '13204',
                                 '13205',
                                 '13206',
                                 '13207',
                                 '13208',
                                 '13209',
                                 '13210',
                                 '13211',
                                 '13212',
                                 '13214',
                                 '13215',
                                 '13217',
                                 '13219',
                                 '13020',
                                 '13224',
                                 '13244',
                                 '13027',
                                 '13250',
                                 '13029',
                                 '13030',
                                 '13031',
                                 '13039',
                                 '13041',
                                 '13051',
                                 '13057',
                                 '13060',
                                 '13063',
                                 '13066',
                                 '13077',
                                 '13078',
                                 '13080',
                                 '13082',
                                 '13084',
                                 '13088',
                                 '13090',
                                 '13104',
                                 '13108',
                                 '13110',
                                 '13112',
                                 '13116',
                                 '13119',
                                 '13120',
                                 '13122')
#Read in zip code shapefile and filter for only Onondaga county, and read in city boundary
onondaga_county_zip_codes <- sf::read_sf("/Users/samedelstein/Downloads/tl_2017_us_zcta510/tl_2017_us_zcta510.shp") %>%
  filter(ZCTA5CE10 %in% zip_codes)

city_boundary <- sf::read_sf("/Users/samedelstein/Downloads/Syracuse_City_Boundary-shp/Syracuse_City_Boundary.shp")

#since the loans are a range, assign the low and high value to each row
onondaga_county_ppp <- PPP %>%
  filter(Zip %in% zip_codes) %>%
  mutate(low_loan = case_when(str_detect(LoanRange, "^a") ~ 5000000,
                              str_detect(LoanRange, "^b") ~ 2000000,
                              str_detect(LoanRange, "^c") ~ 1000000,
                              str_detect(LoanRange, "^d") ~ 350000,
                              str_detect(LoanRange, "^e") ~ 150000),
         high_loan = case_when(str_detect(LoanRange, "^a") ~ 10000000,
                               str_detect(LoanRange, "^b") ~ 5000000,
                               str_detect(LoanRange, "^c") ~ 2000000,
                               str_detect(LoanRange, "^d") ~ 1000000,
                               str_detect(LoanRange, "^e") ~ 350000),
         low_per_job = low_loan/JobsRetained,
         high_per_job = high_loan/JobsRetained) 

#Scatterplot for jobs retained vs loan amount
ggplot(onondaga_county_ppp, aes(low_loan, JobsRetained)) +
  geom_jitter() +
  scale_x_continuous(labels = comma) +
  labs(title = "Jobs Retained vs Loan Amount Received from PPP (low end)",
       x = "Loan Amount from PPP in Dollars (low end)",
       y = "Jobs Retained Report in PPP")+
  ggthemes::theme_economist()

#histogram for jobs retained
ggplot(onondaga_county_ppp, aes(JobsRetained)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Number of Jobs Retained by Organizations Receiving PPP Funds",
       x = "Jobs Retained Report in PPP",
       y = "Count") +
  ggthemes::theme_economist()

###Which Zip Codes Received the most PPP dollars###

PPP_by_zip <- onondaga_county_ppp %>%
  group_by(Zip) %>%
  summarise(sum_high_loan = sum(high_loan),
            sum_low_loan = sum(low_loan),
            count = n()) %>%
  mutate(avg_high_loan = sum_high_loan/count,
         avg_low_loan = sum_low_loan/count)

zip_ppp <- merge(onondaga_county_zip_codes, PPP_by_zip, by.x = "ZCTA5CE10", by.y = "Zip")
ggplot() +
  geom_sf(data = zip_ppp, aes(fill = sum_low_loan), color = "black") +
  geom_sf(data = city_boundary, color = "steelblue", size = 1, alpha = .1)+
  scale_fill_gradient(low = "white", high = "red", name = "Low Total Dollars Received", labels = comma) + 
  xlab("") + ylab("") + labs(title = "Total PPP Loan By Zip Code in Onondaga County") +
  theme(panel.background = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#Which zip codes had the highest average loan amount
ggplot() +
  geom_sf(data = zip_ppp, aes(fill = avg_low_loan), color = "black") +
  geom_sf(data = city_boundary, color = "steelblue", size = 1,  alpha = .1) +
  scale_fill_gradient(low = "white", high = "red", name = "Low Average Dollars Received", labels = comma) + 
  xlab("") + ylab("") + labs(title = "Average PPP Loan By Zip Code in Onondaga County") +
  theme(panel.background = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


