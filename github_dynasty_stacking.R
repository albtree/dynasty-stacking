##For Github
library(ffverse)
library(stringr)
library(rlang)
library(dplyr)
library(tidyverse)
library(purrr)
library(ggpubr)
library(hrbrthemes)
library(ggpmisc)


FF_Leauges3 <- data.frame()
for(i in c("603318787923574784",	"605605240636514304",	"602666544840904704",	"600887455750627328",	"600862958054072320",	"605969842876514304",	"627988822436704256",	"635319081695870976",	"624752882876510208",	"602955298210127872",	"601656231957958656",	"605933865537736704")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_rosters(data)
  df <- data.frame(data2)
  FF_Leauges3<- bind_rows(FF_Leauges3,df)
}

FF_Leauges4 <- data.frame()
for(i in c("600513380213653504",	"624679625431257088",	"602156061243285504",	"602025782201618432",	"602551081377009664",	"600817633390030848",	"603803936528347136",	"604841962805194752",	"602579260774871040",	"605842380700082176",	"604334048776609792",	"605110469526646784",	"627912776697868288",	"604743120818597888",	"637112335764369408",	"600797380945702912",	"600500447576391680",	"635659656626257920",	"600858147158040576",	"632834653161693184",	"602326740538302464",	"600190962018344960",	"604146418143252480",	"608324199362142208",	"603317973427486720",	"600428880695717888",	"608028882808938496",	"623762292324020224",	"600518172264689664",	"606544173721526272",	"603317565183307776",	"602691603479470080",	"605932231403343872",	"603669701968601088",	"606908914360967168",	"600964785965678592",	"601538282878193664",	"600932856822292480",	"625472466789175296",	"600477961509990400",	"629799890767118336",	"602625395539382272",	"609760869651406848",	"602756124126552064",	"604592088419487744",	"631956643529162752",	"635222115628453888",	"635151671705554944",	"600273669779931136",	"654007313807532032",	"655603558292471808",	"632706795277758464",	"642881605513330688",	"645730345077006336",	"646027285861834752",	"649782203789701120",	"649749604501401600",	"650774594076098560",	"650854855740571648",	"650893183697403904",	"650927794569728000",	"653218739680030720",	"653398719651205120",	"653693759728496640",	"653772185722236928",	"653844877401989120",	"654163970411819008",	"654564633234976768",	"656220313687785472",	"656143116415377408",	"654940711401136128",	"656356752295571456",	"656626309811761152",	"657611836325195776",	"657613045375266816")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_rosters(data)
  df <- data.frame(data2)
  FF_Leauges4<- bind_rows(FF_Leauges4,df)
}

FF_Leauges5 <- data.frame()
for(i in c("657614272897060864",	"657706865051901952",	"657858459475206144",	"658045697605570560",	"658589672456392704",	"658906952121790464",	"659482129209561088",	"659820879013810176",	"660145360097832960",	"660532115179413504",	"660604129667805184",	"660696724792229888",	"663253480357560320",	"660959581341577216",	"661815358855942144",	"661984218036908032",	"662280306639835136",	"662744715145310208",	"662807267954995200",	"663011635593572352",	"663425177207504896",	"663783178967781376",	"666366770122158080",	"666721233991168000",	"667199746749853696",	"668293316386844672",	"668298336893210624",	"668686184058261504",	"668722445053677568",	"668757155481989120",	"668915036529455104",	"667075140760240128",	"669951502558789632",	"669214139599659008",	"650514971540668416",	"647575463165771776",	"664642886767296512",	"668933078722351104",	"666119006385704960",	"671820692878069760",	"666498868648697856",	"665422299394039808",	"669618248471367680",	"664940485898498048",	"669384376815026176",	"671777351301947392",	"674480227698696192",	"673204376046764032",	"672317015083515904",	"672634951195271168",	"669809191380144128",	"670066219331690496",	"671049800145240064",	"668305454698426368",	"674013807848271872",	"676534533067218944",	"676284585058869248",	"676156815007899648",	"674429559914979328",	"677966483216736256")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_rosters(data)
  df <- data.frame(data2)
  FF_Leauges5 <- bind_rows(FF_Leauges5,df)
}

FF_rosters_total <- rbind(FF_Leauges3,FF_Leauges4,FF_Leauges5)%>% filter(pos == "QB" | pos == "WR" | pos == "RB" | pos == "TE")

df <- FF_rosters_total        

FF_Leauges3 <- data.frame()
for(i in c("603318787923574784",	"605605240636514304",	"602666544840904704",	"600887455750627328",	"600862958054072320",	"605969842876514304",	"627988822436704256",	"635319081695870976",	"624752882876510208",	"602955298210127872",	"601656231957958656",	"605933865537736704")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_standings(data)
  df <- data.frame(data2)
  FF_Leauges3<- bind_rows(FF_Leauges3,df)
}

FF_Leauges4 <- data.frame()
for(i in c("600513380213653504",	"624679625431257088",	"602156061243285504",	"602025782201618432",	"602551081377009664",	"600817633390030848",	"603803936528347136",	"604841962805194752",	"602579260774871040",	"605842380700082176",	"604334048776609792",	"605110469526646784",	"627912776697868288",	"604743120818597888",	"637112335764369408",	"600797380945702912",	"600500447576391680",	"635659656626257920",	"600858147158040576",	"632834653161693184",	"602326740538302464",	"600190962018344960",	"604146418143252480",	"608324199362142208",	"603317973427486720",	"600428880695717888",	"608028882808938496",	"623762292324020224",	"600518172264689664",	"606544173721526272",	"603317565183307776",	"602691603479470080",	"605932231403343872",	"603669701968601088",	"606908914360967168",	"600964785965678592",	"601538282878193664",	"600932856822292480",	"625472466789175296",	"600477961509990400",	"629799890767118336",	"602625395539382272",	"609760869651406848",	"602756124126552064",	"604592088419487744",	"631956643529162752",	"635222115628453888",	"635151671705554944",	"600273669779931136",	"654007313807532032",	"655603558292471808",	"632706795277758464",	"642881605513330688",	"645730345077006336",	"646027285861834752",	"649782203789701120",	"649749604501401600",	"650774594076098560",	"650854855740571648",	"650893183697403904",	"650927794569728000",	"653218739680030720",	"653398719651205120",	"653693759728496640",	"653772185722236928",	"653844877401989120",	"654163970411819008",	"654564633234976768",	"656220313687785472",	"656143116415377408",	"654940711401136128",	"656356752295571456",	"656626309811761152",	"657611836325195776",	"657613045375266816")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_standings(data)
  df <- data.frame(data2)
  FF_Leauges4<- bind_rows(FF_Leauges4,df)
}

FF_Leauges5 <- data.frame()
for(i in c("657614272897060864",	"657706865051901952",	"657858459475206144",	"658045697605570560",	"658589672456392704",	"658906952121790464",	"659482129209561088",	"659820879013810176",	"660145360097832960",	"660532115179413504",	"660604129667805184",	"660696724792229888",	"663253480357560320",	"660959581341577216",	"661815358855942144",	"661984218036908032",	"662280306639835136",	"662744715145310208",	"662807267954995200",	"663011635593572352",	"663425177207504896",	"663783178967781376",	"666366770122158080",	"666721233991168000",	"667199746749853696",	"668293316386844672",	"668298336893210624",	"668686184058261504",	"668722445053677568",	"668757155481989120",	"668915036529455104",	"667075140760240128",	"669951502558789632",	"669214139599659008",	"650514971540668416",	"647575463165771776",	"664642886767296512",	"668933078722351104",	"666119006385704960",	"671820692878069760",	"666498868648697856",	"665422299394039808",	"669618248471367680",	"664940485898498048",	"669384376815026176",	"671777351301947392",	"674480227698696192",	"673204376046764032",	"672317015083515904",	"672634951195271168",	"669809191380144128",	"670066219331690496",	"671049800145240064",	"668305454698426368",	"674013807848271872",	"676534533067218944",	"676284585058869248",	"676156815007899648",	"674429559914979328",	"677966483216736256")){
  data <- ff_connect(platform = "sleeper", 
                     league_id = i,
                     season = 2021, 
                     rate_limit = TRUE)
  data2 <- ff_standings(data)
  df <- data.frame(data2)
  FF_Leauges5 <- bind_rows(FF_Leauges5,df)
}

FF_leagues_standings <- rbind(FF_Leauges3,FF_Leauges4,FF_Leauges5)


## Isolating starting QBs for 2021 season - top ~36 Fantasy Scoring QBs
qbs <- df %>% filter(pos == "QB", franchise_name != "",
                     player_id == "96" |player_id == "4892" |player_id == "138" |player_id == "3161" |player_id == "2306" |player_id == "7591" |player_id == "7538" |player_id == "7585" |player_id == "4943" |player_id == "5870" |player_id == "7523" |player_id == "6768" |player_id == "1049" |player_id == "1234" |player_id == "4984" |player_id == "6797" |player_id == "167" |player_id == "4046" |player_id == "421" |player_id == "3294" | player_id == "6770" |player_id == "6904" |player_id == "5849" |player_id == "1166" |player_id == "2028" | player_id == "4881" |player_id == "1837" |player_id == "7527" |player_id == "24" |player_id == "2711" |player_id == "2152" |player_id == "3163" | player_id == "2549") %>%
  rename(player_name_of_qb = player_name,
         pos_of_qb = pos,
         team_of_qb = team,
         age_of_qb = age)
## Isolating non QBs
non_qbs <- df %>% filter(pos == "WR" | pos == 'TE', franchise_name != "")
## Joining QBs to their pass catchers
both_df <- non_qbs %>%
  left_join(qbs, by = c('franchise_id' = 'franchise_id', 'franchise_name' = 'franchise_name',
                        'team' = 'team_of_qb')) %>%
  distinct(franchise_id, franchise_name, player_id.x, .keep_all = TRUE)

## Summarising and mutating stacks
stack_summary <- both_df %>%
  group_by(franchise_id, franchise_name, team) %>%
  summarise(no_of_stacks = n()) %>%
  ungroup() %>%
  mutate(single_stack = if_else(no_of_stacks == 1, 1, 0),
         double_stack = if_else(no_of_stacks == 2, 1, 0),
         triple_or_more_stack = if_else(no_of_stacks >= 3, 1, 0)) %>%
  group_by(franchise_id, franchise_name) %>%
  summarise(no_of_single_stacks = sum(single_stack, na.rm = TRUE),
            no_of_double_stacks = sum(double_stack, na.rm = TRUE),
            no_of_triple_or_more_stacks = sum(triple_or_more_stack, na.rm = TRUE)) %>%
  ungroup()


## Adding in teams that didn't have stacks
both_df_no_stacks <- non_qbs %>%
  left_join(qbs, by = c('franchise_id' = 'franchise_id', 'franchise_name' = 'franchise_name',
                        'team' = 'team_of_qb')) %>%
  mutate(player_id.y = replace_na(player_id.y, 0),
         no_stack = if_else(player_id.y >0, 1, 0)) %>%
  filter(no_stack == 0) %>%
  distinct(franchise_id, franchise_name, .keep_all = TRUE)

stack_summary_no_stacks <- both_df_no_stacks %>%
  group_by(franchise_id, franchise_name)%>%
  summarise(no_of_no_stacks = n()) %>%
  ungroup()

## More data wrangling so that double and triple stacks were also counted in single stacks, and triple stacks were counted in double stacks
stacks <- bind_rows(stack_summary, stack_summary_no_stacks) %>%
  mutate(no_of_no_stacks = replace_na(no_of_no_stacks, 0),
         no_of_single_stacks = replace_na(no_of_single_stacks, 0),
         no_of_double_stacks = replace_na(no_of_double_stacks, 0),
         no_of_triple_or_more_stacks = replace_na(no_of_triple_or_more_stacks,0)) %>%
  group_by(franchise_id, franchise_name) %>%
  arrange(desc(no_of_single_stacks)) %>%
  distinct(franchise_id, franchise_name, .keep_all = TRUE) %>%
  mutate(single_stacks = no_of_single_stacks + no_of_double_stacks + no_of_triple_or_more_stacks,
         double_stacks = no_of_double_stacks + no_of_triple_or_more_stacks)

## Joining stacks data with standings data

stacks_and_records <- stacks %>%
  left_join(FF_leagues_standings, by = c('franchise_id' = 'franchise_id',
                                         'franchise_name' = 'franchise_name'))  %>%
  drop_na(allplay_winpct)


## Labelling good teams for allplay win pct > 0.675 (~75th percentile) & adding binary stack variables
records_and_stacks <- FF_leagues_standings %>%
  left_join(stacks, by = c('franchise_id' = 'franchise_id',
                           'franchise_name' = 'franchise_name')) %>%
  drop_na(allplay_winpct, no_of_no_stacks, h2h_winpct) %>%
  mutate(winners = case_when(allplay_winpct > 0.675 ~ "Good teams"),
         winners = replace_na(winners, "Bad Teams"),
         good = if_else(allplay_winpct > 0.675, 1, 0),
         binary_one_stack = as.factor(if_else(single_stacks > 0, 1, 0)),
         binary_double_stack = as.factor(if_else(double_stacks > 0, 1, 0)),
         binary_triple_stack = as.factor(if_else(no_of_triple_or_more_stacks > 0, 1,0)))

## Scatter plots of number of stacks vs all play win percentage

records_and_stacks %>% ggplot(aes(x = single_stacks, y = allplay_winpct)) +
  geom_point(colour = "orangered3", size = 1, alpha = 0.8)+
  geom_smooth(method = lm, se = FALSE, color = "yellow2", size = 1) + 
  stat_regline_equation(label.y = 0.85, label.x = 4.8,
                        aes(label = ..rr.label..))+
  stat_fit_glance(method = 'lm',
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x = 5.3, label.y = 0.75)+
  labs(title = "Number of single stacks on a roster doesn't appear to impact All Play Win percentage",
       subtitle = "1,740 Dynasty Teams in 2021                P-value <0.05 indicates significance",
       x = "Number of single stacks (QB + 1 WR or TE from same team) on a roster",
       y = "All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF") +
  theme_ipsum_rc() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
ggsave("stack_plots_1.png", bg = "#ffffff")

records_and_stacks %>% ggplot(aes(x = double_stacks, y = allplay_winpct)) +
  geom_point(colour = "orangered3", size = 1, alpha = 0.8)+
  geom_smooth(method = lm, se = FALSE, color = "yellow2", size = 1) + 
  stat_regline_equation(label.y = 0.85, label.x = 3.5,
                        aes(label = ..rr.label..))+
  stat_fit_glance(method = 'lm',
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x = 3.8, label.y = 0.75)+
  labs(title = "Neither does double stacks",
       subtitle = "1,740 Dynasty Teams in 2021                P-value <0.05 indicates significance",
       x = "Number of Double stacks (QB + 2 WR or TE from same team) on a roster",
       y = "All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF")+
  theme_ipsum_rc()  +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
ggsave("stack_plots_2.png", bg = "#ffffff")

records_and_stacks %>% ggplot(aes(x = no_of_triple_or_more_stacks, y = allplay_winpct))  +
  geom_point(colour = "orangered3", size = 1, alpha = 0.8)+
  geom_smooth(method = lm, se = FALSE, color = "yellow2", size = 1) + 
  stat_regline_equation(label.y = 0.85, label.x = 2.5,
                        aes(label = ..rr.label..))+
  stat_fit_glance(method = 'lm',
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x = 2.7, label.y = 0.75)+
  labs(title = "Or triple stacks",
       subtitle = "1,740 Dynasty Teams in 2021                P-value <0.05 indicates significance",
       x = "Number of Triple (QB + 3 WR or TE from same team) stacks on a roster",
       y = "All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF")+
  theme_ipsum_rc()  +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
ggsave("stack_plots_3.png", bg = "#ffffff")


## T-tests of whether teams had aa single/double/triple stack compared to All Play Win percentage
res <- t.test(allplay_winpct ~ binary_one_stack, data = records_and_stacks)
res

res <- t.test(allplay_winpct ~ binary_double_stack, data = records_and_stacks)
res

res <- t.test(allplay_winpct ~ binary_triple_stack, data = records_and_stacks)
res

mean_allplay_df1 <- records_and_stacks %>%
  group_by(binary_one_stack) %>%
  summarise(mean_allplay = mean(allplay_winpct)) %>%
  ggplot(aes(x = binary_one_stack, y = mean_allplay)) +
  geom_col(aes(fill = binary_one_stack))+
  labs(title = "Single Stacks",
       x = "Single Stacks Present or Not",
       y = "Mean All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF") +
  theme_ipsum_rc() +
  theme(legend.position = "none", plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
print(mean_allplay_df1)
ggsave("mean_allplay_plots_1.png", bg = "#ffffff")

mean_allplay_df2 <- records_and_stacks %>%
  group_by(binary_double_stack) %>%
  summarise(mean_allplay = mean(allplay_winpct)) %>%
  ggplot(aes(x = binary_double_stack, y = mean_allplay)) +
  geom_col(aes(fill = binary_double_stack))+
  labs(title = "Double Stacks",
       x = "Doublele Stacks Present or Not",
       y = "Mean All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF") +
  theme_ipsum_rc() +
  theme(legend.position = "none", plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
print(mean_allplay_df2)
ggsave("mean_allplay_plots_2.png", bg = "#ffffff")

mean_allplay_df3 <- records_and_stacks %>%
  group_by(binary_triple_stack) %>%
  summarise(mean_allplay = mean(allplay_winpct)) %>%
  ggplot(aes(x = binary_triple_stack, y = mean_allplay))  +
  geom_col(aes(fill = binary_triple_stack))+
  labs(title = "Triple Stacks",
       x = "Triple Stacks Present or Not",
       y = "Mean All Play Win %",
       caption = "Data = ffverse. Author = @TAlbTree. Credit also to @EdnaBEASTmode and @Adeiko_FF") +
  theme_ipsum_rc() +
  theme(legend.position = "none", plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
print(mean_allplay_df3)
ggsave("mean_allplay_plots_3.png", bg = "#ffffff")
