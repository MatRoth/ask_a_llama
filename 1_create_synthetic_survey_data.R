library(reticulate)
library(tidyverse)
library(haven)
library(logger)
library(readxl)


use_condaenv("ask_a_llama_conda_env") # Use conda env from git repository

transformers <- import('ctransformers') # for running llama2 on CPU
afclm <- transformers$AutoModelForCausalLM

softmax <- function(logits){
  sum_of_exp_logits <- sum(exp(logits))
  exp(logits)/sum_of_exp_logits
}

# ALLBUS 2018 cross-section public use file
# https://search.gesis.org/research_data/ZA5270
alb_full <- read_sav("ZA5270_v2-0-0.sav") 
questions <- read_excel("prompts_new.xlsx") # Covariate/prompt questions

alb <- read_sav("ZA5270_v2-0-0.sav") |>
  mutate(nation = "Deutschland",year = "2018")|>
  filter((educ >0 & educ < 7))|>
  select(any_of(questions$var_name),edu = educ,income = di01a)|>
  na.omit()|>
  mutate(sex = if_else(sex == 1,"männlich","weiblich"),
         edu = if_else(edu<4,"Ich habe das Abitur nicht abgeschlossen","Ich habe das Abitur abeschlossen"),
         age = if_else(age >= 16,age,NA),
         income = if_else(income >= 0,income, NA))

# functions to create prompts in llama2 format
add_interviewer <- function(question){
  paste0("Interviewer: ",question)
}
add_llama_inst <- function(interviewer_question){
  paste0("[INST] \n\n ",interviewer_question,"\n\n[/INST] Ich:")
}

promptify <- function(question){
  question |> add_interviewer() |> add_llama_inst()
}

prompts <- questions|>
  mutate(question_prompt = question |> promptify(),
         question_prompt_data = map2(question_prompt,var_name,\(cur_prompt,cur_var){
           paste0(cur_prompt," ",as.character(alb|>pull(cur_var)),". ")
         }))

prompt_lengths <- map(1:12,\(n_row) prompts[1:n_row,] )

results <- map2(prompt_lengths,1:12,\(cur_prompts,i){ # Iterate through prompts length 1 to 12
  indiv_prompts <- cur_prompts$question_prompt_data |>reduce(bind_cols)|>as_tibble()|>pmap(\(...) paste(list(...),collapse = ""))|>as.character()
  
  question_to_ask <- paste("Wie zufrieden sind Sie - insgesamt betrachtet - mit den gegenwärtigen Leistungen der Bundesregierung?",
                           "Die Antwortoptionen sind '1' = Sehr zufrieden, '2' = Ziemlich zufrieden, '3' = Etwas zufrieden, '4' = Etwas unzufrieden, '5' = Ziemlich unzufrieden, '6' = Sehr unzufrieden.")
  resp_opt <- 1:6 |> as.character()
  
  
  indiv_prompts_final <- map_chr(indiv_prompts,paste,promptify(question_to_ask))
  
  alb <- alb |>
    mutate(prompt = indiv_prompts_final)
  
  model_link <- c("TheBloke/Llama-2-13B-chat-GGUF",
                  "TheBloke/Llama-2-13B-GGUF",
                  "TheBloke/leo-hessianai-13B-chat-GGUF",
                  "TheBloke/leo-hessianai-13B-GGUF")
  model_spec <- c("llama-2-13b-chat.Q8_0.gguf",
                  "llama-2-13b.Q8_0.gguf",
                  "leo-hessianai-13b-chat.Q8_0.gguf",
                  "leo-hessianai-13b.Q8_0.gguf")
  
  
  t <- getwd() # logging to current working directory
  log_appender(appender_tee(t))
  
  res<- map2(model_link,model_spec,\(cur_link,cur_spec){ # iterate through models in current prompt length
    log_info(paste("Working on",cur_link,"n Background infos", i,"ALLBUS rows:",min(selected_rows),"to",max(selected_rows)))
    cur_model <- afclm$from_pretrained(cur_link,model_file=cur_spec,model_type="llama",context_length=2048L) 
    results <- alb_subset |>
      mutate(model_results = map(prompt,\(cur_prompt){ #iterate through prompts with current model in current prompt length
        cur_model$reset() # reset model for new prompt
        cur_model$eval(cur_model$tokenize(cur_prompt)) # evaluate current prompt with current model
        # iterate through response options for current prompt with current model with current prompt length
        logprobs <- map_dbl(map_int(resp_opt,\(cur_resp) cur_model$tokenize(cur_resp,add_bos_token = F)[2]),cur_model$logits$"__getitem__") 
        probs <- logprobs |>softmax()
        
        all_logits <- map_dbl(as.integer(0:(32000-1)),cur_model$logits$"__getitem__")
        #browser()
        list(logprobs = logprobs,probs = probs,all_logits = all_logits)
      }))
    log_info(paste0("Work on",cur_link,"completed."," n Background infos", i))
    #browser()
    write_rds(results,paste0(getwd(),"/",str_replace(cur_link,"/","_"),"_n_info_",i,"_.rds"))
    results})
  res})
