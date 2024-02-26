library(tidyverse)
library(haven)
library(readxl)
library(ggtext)
data <- read_rds("complete_unpacked_probs_maxlogit.rds")

#data <- data |> rename(model_results = probs)

questions <- read_excel("prompts_new.xlsx")
alb <- read_sav("ZA5270_v2-0-0.sav") |>
  mutate(nation = "Deutschland",year = "2018")|>
  filter((educ >0 & educ < 7))|>
  select(any_of(questions$var_name),edu = educ,income = di01a,ps01)|>
  na.omit()|>
  mutate(sex = if_else(sex == 1,"männlich","weiblich"),
         edu = if_else(edu<4,"Ich habe das Abitur nicht abgeschlossen","Ich habe das Abitur abeschlossen"),
         age = if_else(age >= 16,age,NA),
         income = if_else(income >= 0,income, NA)) |> 
  slice_head(n=1000)

data<-data|>
  group_by(model,n_info)|>
  nest()|>
  mutate(data = map(data,\(cur_data) mutate(cur_data,ps01 = alb$ps01)))|>
  unnest(data) |> 
  mutate(highest_prob = map_dbl(probs,\(x) max(x))) |> 
  mutate(most_likely = map_int(probs,\(x) which(x == max(x))),
         highest_prob = map_dbl(probs,\(x) max(x)),
         error = abs(most_likely-ps01))

most_likely_data <- data |>
  mutate(most_likely = map_int(probs,\(x) which(x == max(x))),
         highest_prob = map_dbl(probs,\(x) max(x)),
         error = abs(most_likely-ps01)) |>
  group_by(model,n_info) |> 
  summarise(mean_logit = mean(logits,na.rm = T),
            mean_highest_prob = mean(highest_prob),
            mean_model = mean(most_likely),
            sd_model = sd(most_likely),
            mean_error = abs(mean(most_likely)-mean(ps01)),
            sd_error = abs(sd(most_likely)-sd(ps01)),
            cor = cor(most_likely,ps01),
            cohens_d = (mean(most_likely)-mean(ps01))/sd(ps01),
  )


#logit analysis
data |>
  group_by(model,n_info) |>
  nest() |>
  mutate(data = map(data,\(x) mutate(x,scaled_to_model_logits = scale(logits)))) |>
  unnest(data)|>
  mutate(most_likely = map_int(probs,\(x) which(x == max(x))),
         highest_prob = map_dbl(probs,\(x) max(x)),
         error = abs(most_likely-ps01)) |>
  ggplot(aes(scaled_to_model_logits,error,color = model))+
  geom_point()+
  geom_smooth()+
  facet_wrap(vars(n_info))


# model lookuptable
model_lookup<-data$model |> unique() |> set_names(c("🦙", 
                                                    "🦙🗨",
                                                    "🦙DE",
                                                    "🦙DE🗨"))

#Figure 1 SD error ----
sd_plot<-data |>
  mutate(sim = map(probs,\(x) sample(1:6,100,replace = T,prob = x))) |> 
  unnest(sim) |> 
  group_by(model,n_info) |> 
  summarise(mean_logit = mean(logits,na.rm = T),
            mean_highest_prob = mean(highest_prob),
            mean_model = mean(sim),
            sd_model = sd(sim),
            mean_error = abs(mean(sim)-mean(ps01)),
            sd_error = abs(sd(sim)-sd(ps01)),
            cor = cor(sim,ps01),
            cohens_d = (mean(sim)-mean(ps01))/sd(ps01)) |>
  mutate(type = "Sampling from\nresponse\ndistribution",
         model = map_chr(model,\(cur_model) names(model_lookup[cur_model == model_lookup])) |> 
           factor(levels = c("🦙", 
                             "🦙🗨",
                             "🦙DE",
                             "🦙DE🗨"))) |> 
  ungroup() |> 
  bind_rows(most_likely_data |>
              mutate(type = "Most likely\nresponse",
                     model = map_chr(model,\(cur_model) names(model_lookup[cur_model == model_lookup]))|> 
                       factor(levels = c("🦙", 
                                         "🦙🗨",
                                         "🦙DE",
                                         "🦙DE🗨")))) |> 
  ggplot(aes(n_info,sd_model,linetype=type,color = model))+
  geom_point()+
  geom_line(size = 2)+
  geom_hline(aes(yintercept = sd(alb$ps01)))+
  geom_vline(aes(xintercept = 6.5),size = 2)+
  theme_classic(base_size = 45)+
  labs(x = "Number of covariates",
       y = "Standard deviation",
       color = "Model",
       linetype = "Response\ngeneration",
       subtitle = "Effect of the number of covariates on\nthe standard deviation of synthetic data")+
  theme()+
  guides(color=guide_legend(nrow=1))+
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))+
  scale_x_continuous(breaks = 0:12)+
  scale_y_continuous(limits = c(-0.2,1.65))+
  geom_text(aes(x=3.5,y=-0.2,label = "Socio-demographic covariates"),size = 11,show.legend = F,color = "black")+
  geom_text(aes(x=9,y=-0.2,label = "Substantive covariates"),size = 11,show.legend = F,color = "black")+
  annotate("text",x=0,y=sd(alb$ps01),label = "True\nSD",size = 9,color = "black",angle = 0)+
  coord_cartesian(xlim = c(1,12),  clip = 'off')+
  theme(legend.key.size = unit(2,"line"))


sd_plot
#ggsave("analysis/figures/sd_figure.jpg",plot = sd_plot,width = 16,height = 15)

#mean response
data |>
  mutate(sim = map(probs,\(x) sample(1:6,100,replace = T,prob = x))) |> 
  unnest(sim) |> 
  group_by(model,n_info) |> 
  summarise(mean_model = mean(sim),
            sd_model = sd(sim),
            mean_error = abs(mean(sim)-mean(ps01)),
            sd_error = abs(sd(sim)-sd(ps01)),
            cor = cor(sim,ps01),
            cohens_d = (mean(sim)-mean(ps01))/sd(ps01)) |>
  mutate(type = "Simulated responses") |> 
  bind_rows(most_likely_data |> mutate(type = "Most likely response")) |> 
  ggplot(aes(n_info,cohens_d,linetype=type))+
  geom_point()+
  geom_smooth(se = F,alpha = 0.1)+
  geom_hline(aes(yintercept = 0))+
  theme_classic(base_size = 18)+
  facet_wrap(vars(model))+
  labs(x = "Number of covariates",
       y = "Standardized mean difference\n(Cohens d)",
  )+
  theme(legend.position = "bottom",
        plot.subtitle = element_markdown())

#correlation
questions$var_name -> covariates
formulas<-tibble(n_info = 1:12,covariates=list(covariates))|>
  mutate(covariates_split = covariates|>map2(n_info,\(cur_cov,cur_n_info) {cur_cov[1:cur_n_info]|> discard(\(x) x == "nation" | x == "year")}),
         covariates = map(covariates,paste,collapse = "+"),
         formulas = map(covariates,\(cur_cov) paste0("sim~",cur_cov,collapse = "") |> as.formula()))



reg_form_true <- paste0("ps01~",covariates|> discard(\(x) x == "nation" | x == "year")|> paste(collapse = "+"),collapse = "") |> as.formula()
true_cor <- lm(ps01~(.),
               alb|>
                 select(ps01,all_of(formulas$covariates_split[12][[1]]))|>
                 mutate(sex = factor(sex,levels = c("männlich","weiblich"))))|>
  effectsize::standardize()|>
  broom::tidy()

alb|>
  select(all_of(questions$var_name |> discard(\(x) x == "nation" | x == "year")),ps01) |>
  mutate(across(is.character,\(y) y |>
                  as.factor() |>
                  as.numeric())) |> cor()

cor_res_all <- data |> 
  mutate(sim = map(probs,\(x) sample(1:6,200,replace = T,prob = x)),
         chat = str_detect(model,"chat")) |> 
  group_by(model,chat,n_info) |> 
  nest() |> 
  left_join(formulas, by = "n_info")|>
  mutate(data = map(data,\(x) x |> unnest(sim)),
         lm_res = map2(data,covariates_split,\(cur_data,cur_vars){ 
           #browser()
           lm(sim~(.),
              cur_data|>
                select(sim,all_of(cur_vars))|>
                mutate(sex = factor(sex,levels = c("männlich","weiblich"))))|>
             effectsize::standardize() |>
             broom::tidy()}),
         cor_mat = map(data,\(x) x |>
                         select(all_of(questions$var_name |> discard(\(x) x == "nation" | x == "year")),ps01,sim) |>
                         mutate(across(is.character,\(y) y |>
                                         as.factor() |>
                                         as.numeric())) |> cor()),
         sim_cor = map(cor_mat,\(x) x[,ncol(x)])) |> unnest(lm_res)# |>
# select(-where(is.list))

cor_res <- cor_res_all |> select(-where(is.list))
cor_res_matrix <- cor_res_all |> distinct(model,n_info,.keep_all = T)
rm(cor_res_all)

#Covariate lookup
cov_lookup<-set_names(true_cor$term,
                      nm = c("(Intercept)",
                             "Female",
                             "Age",
                             "Low\nEducation",
                             "Income",
                             "Political\ninterest",
                             "Polit. should\nbe normal\ncitizens",
                             "Political\nactivity",
                             "Politics to\ncomplicated",
                             "Subjective class",
                             "Politicians\ndon't care"))

# Figure 2 - correlation error -----
cor_plot <- true_cor |>
  select(term,estimate_true = estimate,std.error.true = std.error) |>
  right_join(cor_res) |>
  mutate(term = map_chr(term,\(cur_term) names(cov_lookup[cur_term == cov_lookup])) |> 
           factor(levels = names(cov_lookup)),
         model = map_chr(model,\(cur_model) names(model_lookup[cur_model == model_lookup]))|> 
           factor(levels = c("🦙", 
                             "🦙🗨",
                             "🦙DE",
                             "🦙DE🗨"))) |> 
  # filter(term != "(Intercept)")|> 
  ggplot(aes(y = term, x = estimate,color = model,label = if_else(!(-0.25<estimate & estimate<0.2),n_info,NA)))+
  geom_jitter(width = 0.01,height = 0.3,size = 6)+
  geom_text(check_overlap = T,size = 15,show.legend = F)+
  geom_pointrange(aes(y = term,x = estimate_true,xmin =estimate_true-std.error.true*1.96,xmax = estimate_true+std.error.true*1.96),color = "blue")+
  theme_classic(base_size = 53)+
  labs(y = "Covariates",
       x = "Estimated correlation coefficient",
       color = "Model",
       subtitle = "Mult. regression of synthetic data<br/>on covariates (true cor. <span style='color: blue;'>blue</span>)")+
  theme(legend.position = "bottom")+
  theme(
    axis.title.x= element_text(hjust = 1))+
  geom_vline(aes(xintercept = 0))+
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.justification = 1,
        plot.subtitle = element_markdown(size = 48))

cor_plot
#ggsave("cor_figure.jpg",plot = cor_plot,width = 16,height = 22)

true_cor |>
  select(term,estimate_true = estimate,std.error.true = std.error) |>
  right_join(cor_res) |>
  filter(term != "(Intercept)")|> 
  ggplot(aes(n_info,abs(estimate-estimate_true),color = term))+
  geom_jitter(width = 0.1,size = 3)+
  #geom_smooth(method = "lm", se = F)+
  scale_x_continuous(breaks = 1:12)
