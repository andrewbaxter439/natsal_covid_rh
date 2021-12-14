cross_tab_by_var <- function(variable, col_head = "Variable", data = lumped_data, output = c("tibble", "gt")) {
  
  output <- match.arg(output)
  
  table <- data %>% 
    group_by({{variable}}, resp_age_grps) %>% 
    summarise(n = n()) %>% 
    mutate(all = sum(n),
           perc = n/all) %>% 
    rowwise() %>% 
    mutate(ci_l = prop.test(n, all)$conf.int[[1]],
           ci_u = prop.test(n, all)$conf.int[[2]],
           across(c(perc, ci_l, ci_u), ~paste0(as.character(round(.x*100, 1)), "%")),
           perc_label = paste0(perc, " (", ci_l, "-", ci_u, ")")) %>% 
    ungroup() %>%
    select({{col_head}} := {{variable}}, resp_age_grps, n, `All ages` = all, "% (95%CI)" = perc_label) %>% 
    pivot_wider(names_from = c("resp_age_grps"), values_from = c("n", "% (95%CI)"), names_glue = "{resp_age_grps}_{.value}")
  
  if (output == "gt") {
    table %>% 
      gt(rowname_col = colnames(select(data, {{variable}}))) %>% 
      tab_spanner_delim(delim = "_") %>%
      cols_move_to_end(`All ages`)
  } else {
    table
  }
}



combine_csv_outputs <- function(exposure) {
  # exposure <- "D_Age5Cat_w2"
  
  imports <-
    dir() %>%
    str_subset(exposure) %>%
    str_subset("_by") %>% 
    tibble(
      file = .,
      outcome = str_extract(file, ".*(?=_by)"),
      metric = str_extract(file, paste0("(?<=", exposure, "-).*(?=\\.csv)"))
    )
  
  imports %>%
    group_by(outcome) %>%
    group_map(function(outcome, key) {
      # outcome <- imp_sample %>% filter(outcome == "AccDisPan_w2")
      tables <- c("prop", "n", "wn") %>%
        set_names() %>%
        map(function(met) {
          outcome %>%
            filter(metric == met) %>%
            pull(file) %>%
            read_csv(skip = 1)
        })
      
      p_val <- str_extract_all(tables$prop, "(?<=Pr = )[0-9.]*") %>%
        reduce(c) %>%
        as.numeric()
      
      t_prop <- tables$prop %>%
        filter(!is.na(Total)) %>%
        mutate(across(1, replace_na, "Metric")) %>%
        pivot_longer(-1, names_to = "age_grp", values_to = "val") %>%
        rename(response = 1) %>%
        pivot_wider(age_grp, names_from = response, values_from = val) %>%
        mutate(age_grp = str_remove(age_grp, "_1$")) %>%
        pivot_longer(c(-1,-2), names_to = "response", values_to = "val") %>%
        pivot_wider(c(age_grp, response),
                    names_from = "Metric",
                    values_from = val) %>%
        mutate(
          `Weighted prop` = as.numeric(`Weighted prop`),
          perc = paste0(sprintf(
            "%#.1f", `Weighted prop` * 100
          ), "%"),
          lower_ci = as.numeric(str_extract(CI, "(?<=\\[)[0-9\\.]*")),
          upper_ci = as.numeric(str_extract(CI, "(?<=,)[0-9\\.]*")),
          across(
            c(upper_ci, lower_ci),
            ~ ifelse(response == "Total", "", paste0(sprintf("%#.1f", .x * 100), "%"))
          ),
          `95%CI` = ifelse(
            response == "Total",
            "",
            paste0("(", lower_ci, ", ", upper_ci, ")")
          )
        )
      
      t_n <-  tables$n %>%
        mutate(across(-1, as.numeric)) %>%
        drop_na() %>%
        rename(response = 1) %>%
        pivot_longer(-1, names_to = "age_grp", values_to = "N")
      
      t_wn <- tables$wn %>%
        mutate(across(-1, as.numeric)) %>%
        drop_na() %>%
        rename(response = 1) %>%
        pivot_longer(-1, names_to = "age_grp", values_to = "N_w")
      
      outtab <- list(t_prop, t_n, t_wn) %>%
        reduce(left_join, by = c("age_grp", "response")) %>%
        mutate(Denominators = paste(N, N_w, sep = ","),
               p_val = p_val,
               variable = colnames(tables$prop[1]))
      
      list(variable = key$outcome, df = outtab)
    })
  
  
}

# For 'stacked' outcomes (when several output tables are combined)
combine_csv_stacked <- function(exposure, n_responses = 2, cat_labels = NULL, groups = 3) {
  
  imports <-
    dir() %>%
    str_subset(exposure) %>%
    str_subset("_stacked") %>% 
    tibble(
      file = .,
      outcome = str_extract(file, ".*(?=_stacked)"),
      metric = str_extract(file, paste0("(?<=", exposure, "-)\\w*(?=\\.csv)"))
    ) %>% filter(!is.na(metric))
  
  imports %>%
    group_by(outcome) %>%
    group_map(function(outcome, key) { 
      
      all_na <- function(x) sum(!is.na(x)) == 0
      
      tables <- c("prop", "n", "wn") %>%
        set_names() %>%
        map(function(met) {
          outcome %>%
            filter(metric == met) %>%
            pull(file) %>%
            read_csv(col_names = paste0("x", 1:14), skip = 0) %>%
            janitor::remove_empty(which = "rows")
        })
      
      r_s <- n_responses + 6
      r_f <- n_responses + 4
      
      t_p <- map_dfr(1:(nrow(tables$prop)/r_s), ~ {
        # map_dfr(2, ~ {
        tables$prop[(r_s*.x-(r_s-2)): (r_s*.x),] %>% 
          `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
          select(where(~!all_na(.x))) %>% 
          mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
                 outcome = ifelse(is.null(cat_labels), str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$")), cat_labels[.x])) %>%
          fill(outcome, .direction = "down") %>% 
          mutate(p_val = str_extract(.[[2]], "(?<=Pr = ).*")) %>% 
          fill(p_val, .direction = "up")  %>% 
          filter(across(1, ~str_detect(.x, "Yes"))) %>%
          select(outcome, everything(), -1, -filt) 
      }) %>% select(where(~!all_na(.x)))
      
      
      t_n <- map_dfr(1:(nrow(tables$n)/r_f), ~ {
        tables$n[(r_f*.x-(r_f-2)): (r_f*.x),] %>% 
          `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
          select(where(~!all_na(.x))) %>% 
          mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
                 outcome = ifelse(is.null(cat_labels), str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$")), cat_labels[.x])) %>%
          fill(outcome, .direction = "down") %>% 
          filter(across(1, ~str_detect(.x, "Yes"))) %>%
          select(outcome, everything(), -1, -filt) 
      }) %>% select(where(~!all_na(.x)))
      
      
      t_wn <- map_dfr(1:(nrow(tables$wn)/r_f), ~ {
        tables$wn[(r_f*.x-(r_f-2)): (r_f*.x),] %>% 
          `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
          select(where(~!all_na(.x))) %>% 
          mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
                 outcome = ifelse(is.null(cat_labels), str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$")), cat_labels[.x])) %>%
          fill(outcome, .direction = "down") %>% 
          filter(across(1, ~str_detect(.x, "Yes"))) %>%
          select(outcome, everything(), -1, -filt) 
      }) %>% select(where(~!all_na(.x)))
      
      
      outtab <- list(t_p, t_n, t_wn) %>% 
        reduce(left_join, by = "outcome") %>% 
        pivot_longer(-c(1, p_val), names_to = "var_metric", values_to = "val") %>% 
        separate(var_metric, into = c("age_grp", "metric"), sep = "_") %>% 
        unique() %>% 
        pivot_wider(names_from = "metric", values_from = val) %>% 
        mutate(
          `Weighted prop` = as.numeric(`Weighted prop`),
          perc = paste0(sprintf(
            "%#.1f", `Weighted prop` * 100
          ), "%"),
          lower_ci = as.numeric(str_extract(CI, "(?<=\\[)[0-9\\.]*")),
          upper_ci = as.numeric(str_extract(CI, "(?<=,)[0-9\\.]*")),
          across(
            c(upper_ci, lower_ci), ~ paste0(sprintf("%#.1f", .x * 100), "%")
          ),
          `95%CI` =  paste0("(", lower_ci, ", ", upper_ci, ")"),
          Denominators = paste(N, `Weighted n`, sep = ","),
          variable = key$outcome)# str_trim(str_extract(colnames(tables$prop[1]), "(?<=:).*(?=:?)")))
      
      list(variable = key$outcome, df = outtab)
    })
  
}

write_to_xlsx <-
  function(workbook,
           unstacked_imports = NULL,
           stacked_imports = NULL, tidy_names = TRUE) {
    path <- file.path("Output xlsx", workbook)
    cat("opening ", path)
    wb <- loadWorkbook(path)
    
    if(!is.null(unstacked_imports)) {
      
    unstacked_imports %>%
      walk( ~ {
        if (.x$variable %in% getSheetNames(path)) {
          removeWorksheet(wb, .x$variable)
        }
        addWorksheet(wb, .x$variable)
        order <- unique(.x$df$age_grp)
        df_unordered <- .x$df %>%
          mutate(`95%CI` = ifelse(`Weighted prop` == 0 | `Weighted prop` == 1, "", `95%CI`)) %>%
          select(variable,
                 response,
                 age_grp,
                 perc,
                 `95%CI`,
                 Denominators,
                 p_val) %>%
          filter(response != "No") %>%
          mutate(
            perc = ifelse(response != "Total", perc, ""),
            p_val = case_when(
              response == "Total" ~ "",
              p_val == 0 ~ "<0.001",
              TRUE ~ as.character(p_val)
            )
          ) %>%
          pivot_wider(
            c(response, p_val),
            names_from = age_grp,
            values_from = c(perc, `95%CI`, Denominators),
            names_glue = "{age_grp};{.value}"
          ) #%>%
        
        
        order_colnames <- vector("character", ncol(df_unordered))
        
        order %>%
          map( ~ {
            str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
          }) %>%  reduce(c) %>%
          select(df_unordered, response, ., p_val) %>%
          writeData(wb, sheet = .x$variable, x = .)
      })
    
    saveWorkbook(wb, path, overwrite = TRUE)
    
    }
    
    # doing it with files with multiple responses -----------------------------
    
    
    
    if(!is.null(stacked_imports)) {
      
    if(tidy_names) {
      
    labs_replace <-
      tibble(
        name = c(
          "The place where I usually get my contraception was closed/unavailable",
          "My usual method of preventing pregnancy wasnt available",
          "A health professional suggested I use a different method",
          "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
          "I could not get to a GP/clinic/pharmacy because of Covid-19",
          "I didnt want to bother the NHS during the pandemic",
          "Another reason",
          "GP/Clinic/pharmacy I usually go to was closed",
          "My appointment was cancelled / I couldn't get an appointment",
          "I could only access my usual GP/clinic/pharmacy by phone/video/online",
          "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
          "I could not get to a GP/clinic/pharmacy because of Covid-19",
          "didnt want to bother the NHS during the pandemic"
        )
      ) %>%
      mutate(match = tolower(str_extract(name, ".{10}"))) %>%
      unique() %>%
      arrange(match)
    
    labs_out <-
      tibble(outcome = unique(
        c(stacked_imports[[3]]$df$outcome, stacked_imports[[1]]$df$outcome)
      )) %>%
      mutate(match = tolower(str_extract(outcome, ".{10}")))
    
    names_replace <-
      labs_out %>% left_join(labs_replace, by = "match") %>%
      select(outcome, name)
    
    imports_w_names <- stacked_imports %>%
      modify(~ {
        .x$df <- .x$df %>%
          left_join(names_replace, by = "outcome", keep = FALSE) %>%
          mutate(name = ifelse(is.na(name), outcome, name))
        .x
      })
    
    } else {
      imports_w_names <- stacked_imports %>% 
        modify( ~{
          .x$df <- .x$df %>% 
            mutate(name = outcome)
          .x
        })
      
    }
    
    # Now writing to xlsx? ----------------------------------------------------
    
    
    wb <- loadWorkbook(path)
    
    
    imports_w_names %>%
      walk(~ {
        if (.x$variable %in% getSheetNames(path)) {
          removeWorksheet(wb, .x$variable)
        }
        addWorksheet(wb, .x$variable)
        order <- unique(.x$df$age_grp)
        df_unordered <- .x$df %>%
          mutate(`95%CI` = ifelse(`Weighted prop` == 0 | `Weighted prop` == 1, "", `95%CI`),
                 p_val = case_when(
                   p_val == 0 ~ "<0.001",
                   TRUE ~ as.character(p_val)
                 )) %>%
          select(variable,
                 response = name,
                 age_grp,
                 perc,
                 `95%CI`,
                 Denominators,
                 p_val) %>%
          pivot_wider(
            c(response, p_val),
            names_from = age_grp,
            values_from = c(perc, `95%CI`, Denominators),
            names_glue = "{age_grp};{.value}"
          ) #%>%
        
        
        order_colnames <- vector("character", ncol(df_unordered))
        
        order %>%
          map(~ {
            str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
          }) %>%  reduce(c) %>%
          select(df_unordered, response, ., p_val) %>%
          writeData(wb, sheet = .x$variable, x = .)
      })
    
    saveWorkbook(wb, path, overwrite = TRUE)
    
    }
  }


robust_glm <- function(df, formula, weights) {
  require(rlang)
  require(sandwich)
  
  mod <-
    eval_tidy(quo(glm(
      formula,
      data = df,
      family = binomial("logit"),
      weights = !!substitute(weights)
    )))
    
  se <- sqrt(diag(vcovHC(mod, type = "HC0")))
  
  tibble::tibble(
    coef = names(coef(mod)),
    est = coef(mod),
    se_robust = se,
    z = est / se,
    p = 2 * pnorm(abs(z), lower.tail = FALSE),
    ll = est - 1.96 * se,
    ul = est + 1.96 * se
  )
}


return_ORs <- function(df, formula, weights) {
  
  cats <- levels(fct_drop(df$Cat)) %>% str_replace_all("(\\(|\\))", "\\\\\\1")
  require(rlang)
  require(sandwich)
  
  mod <-
    eval_tidy(quo(glm(
      formula,
      data = df,
      family = binomial("logit"),
      weights = !!substitute(weights)
    )), data = df)
  
  se <- sqrt(diag(vcovHC(mod, type = "HC0")))
  
  mod_return <- tibble::tibble(
    coef = names(coef(mod)),
    est = coef(mod),
    se_robust = se,
    z = est / se,
    p = 2 * pnorm(abs(z), lower.tail = FALSE),
    ll = est - 1.96 * se,
    ul = est + 1.96 * se
  )
  
  mod_return %>% filter(
    !str_detect(coef, "(Intercept|D_Age)")
  ) %>% 
    mutate(Cat = str_extract(coef, paste0("(", paste(cats, collapse = "|"),")"))) %>% 
    mutate(aOR = round(exp(est), 2),
           CI = paste0("(", round(exp(ll), 2), ", ", round(exp(ul), 2), ")"),
           P = case_when(
             p < 0.001 ~  "<0.001",
             TRUE ~ as.character(round(p, 3))
           )) %>% 
    bind_rows(tibble(Cat = cats[[1]], est = 1), .)
  
}

perc_ci <- function(perc, lim = "l", n) {
  p_t <- log(perc/(1-perc))
  se <-  sqrt(perc*(1-perc)*(1/n))
  
  if (lim == "l") {
    l_t <-  p_t-qt(0.975, n)*se/(perc*(1-perc))
  } else {
    l_t  <-  p_t+qt(0.975, n)*se/(perc*(1-perc))
  }
  l  <-  exp(l_t)/(1+exp(l_t))
  
  l
}