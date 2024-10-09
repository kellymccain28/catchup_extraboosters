coRbine <- function(){
  
  summarized_ageyr_draws_0.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_0.5.rds")
  summarized_ageyr_draws_1.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_1.5.rds")
  summarized_ageyr_draws_1 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_1.rds")
  summarized_ageyr_draws_2.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_2.5.rds")
  summarized_ageyr_draws_2 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_2.rds")
  summarized_ageyr_draws_3.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_3.5.rds")
  summarized_ageyr_draws_3 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_3.rds")
  summarized_ageyr_draws_4.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_4.5.rds")
  summarized_ageyr_draws_4 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_4.rds")
  summarized_ageyr_draws_5.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_5.5.rds")
  summarized_ageyr_draws_5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_5.rds")
  summarized_ageyr_draws_6.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_6.5.rds")
  summarized_ageyr_draws_6 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_6.rds")
  summarized_ageyr_draws_7.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_7.5.rds")
  summarized_ageyr_draws_7 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_7.rds")
  summarized_ageyr_draws_8.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_8.5.rds")
  summarized_ageyr_draws_8 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_8.rds")
  summarized_ageyr_draws_9.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_9.5.rds")
  summarized_ageyr_draws_9 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_9.rds")
  summarized_ageyr_draws_10.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_10.5.rds")
  summarized_ageyr_draws_10 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_10.rds")
  summarized_ageyr_draws_11.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_11.5.rds")
  summarized_ageyr_draws_11 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_11.rds")
  summarized_ageyr_draws_12.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_12.5.rds")
  summarized_ageyr_draws_12 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_12.rds")
  summarized_ageyr_draws_13.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_13.5.rds")
  summarized_ageyr_draws_13 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_13.rds")
  summarized_ageyr_draws_14.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_14.5.rds")
  summarized_ageyr_draws_14 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_14.rds")
  summarized_ageyr_draws_15.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_15.5.rds")
  summarized_ageyr_draws_15 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_15.rds")
  summarized_ageyr_draws_16.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_16.5.rds")
  summarized_ageyr_draws_16 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_16.rds")
  summarized_ageyr_draws_17.5 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_17.5.rds")
  summarized_ageyr_draws_17 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_17.rds")
  summarized_ageyr_draws_18 <- readRDS("R:/Kelly/catchupR21/draft/5.5_process_combined/20231218-141259-caa0d062/data/summarized_ageyr_draws_18.rds")
  
  ageyr <- bind_rows(list(summarized_ageyr_draws_0.5,summarized_ageyr_draws_1,
                          summarized_ageyr_draws_1.5,summarized_ageyr_draws_2,
                          summarized_ageyr_draws_2.5,summarized_ageyr_draws_3,
                          summarized_ageyr_draws_3.5,summarized_ageyr_draws_4,
                          summarized_ageyr_draws_4.5,summarized_ageyr_draws_5,
                          summarized_ageyr_draws_5.5,summarized_ageyr_draws_6,
                          summarized_ageyr_draws_6.5,summarized_ageyr_draws_7,
                          summarized_ageyr_draws_7.5,summarized_ageyr_draws_8,
                          summarized_ageyr_draws_8.5,summarized_ageyr_draws_9,
                          summarized_ageyr_draws_9.5,summarized_ageyr_draws_10,
                          summarized_ageyr_draws_10.5,summarized_ageyr_draws_11,
                          summarized_ageyr_draws_11.5,summarized_ageyr_draws_12,
                          summarized_ageyr_draws_12.5,summarized_ageyr_draws_13,
                          summarized_ageyr_draws_13.5,summarized_ageyr_draws_14,
                          summarized_ageyr_draws_14.5,summarized_ageyr_draws_15,
                          summarized_ageyr_draws_15.5,summarized_ageyr_draws_16,
                          summarized_ageyr_draws_16.5,summarized_ageyr_draws_17,
                          summarized_ageyr_draws_17.5,summarized_ageyr_draws_18))
  
  saveRDS(ageyr, 'R:/Kelly/catchupR21/src/5.5_process_combined/output_ageyr_intermediate.rds')
}