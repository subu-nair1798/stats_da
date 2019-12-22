library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(datasets)
library(shinyalert)
library(shinyjs)
library(pdfetch)
library(caTools)
library(MASS)

shinyServer(
  function(input, output, session) {
    
    output$mnom_xp <- renderUI({
      lapply(1:input$mnom_k , function(i) { 
        strong(numericInput(as.character(paste0("x", i)), paste0("x", i), value = as.numeric(i)), 
               numericInput(as.character(paste0("p", i)), paste0("p", i), value = 0.5),
               br())
        })
    })
    
    observeEvent(input$dpm_gen_btn, {
        
        output$dpm_gen_plot <- renderPlot({
          
          input$dpm_gen_btn
          
          isolate({
            if(input$dpm_gen_input == "Bernoulli") {
              set.seed(1234)
              bern_plot <- rbinom(input$dpm_gen_s, 1, input$bern_p)
              barplot(table(bern_plot), main = "Bar graph for Bernoulli model")
              
            } else if(input$dpm_gen_input == "Binomial") {
              
              set.seed(1234)
              par(mfrow = c(1,2))
              binom_plot <- density(rbinom(input$dpm_gen_s, input$binom_t, input$binom_p))
              plot(binom_plot, main = "Kernel Density of Generated data")
              polygon(binom_plot, col = "red", border = "blue")
              x <- 0:input$binom_t
              plot(x, dbinom(x, input$binom_t, input$binom_p))
              
            } else if(input$dpm_gen_input == "Multinomial") {
              
              set.seed(1234)
              temp1 <- c(-1)
              temp2 <- c(-1)
              if(input$mnom_k > 0) {
                for(i in 1:input$mnom_k) {
                  temp1 <- c(temp1, as.numeric(input[[paste0("p", as.character(i))]]))
                  temp2 <- c(temp2, as.numeric(input[[paste0("x", as.character(i))]]))
                }
              }
              
              temp1 <- temp1[-1]
              temp2 <- temp2[-1]
              sample_size <- sum(temp2)
              mnom_plot <- rmultinom(input$dpm_gen_s, sample_size, temp1)
              df = data.frame(mnom_plot)
              
              if(sample_size%%2 == 0) {
                par(mfrow = c(2, (sample_size/2)))
              } else if(sample_size%%3 == 0) {
                par(mfrow = c(3, round(sample_size/3)))
              } else {
                par(mfrow = c(sample_size/5, 5))
              }
              
              for(i in 1:sample_size) {
                barplot(df[,i], ylim = c(0, sample_size), names.arg = temp1)
              }
              
            } else if(input$dpm_gen_input == "Poisson") {
              
              set.seed(1234)
              par(mfrow = c(1, 2))
              pois_plot <- rpois(input$dpm_gen_s, input$pois_lambda)
              tab <- table(pois_plot)
              barplot(pois_plot, col = "blue")
              x <- 0:input$pois_end
              y <- dpois(x, input$pois_lambda)
              plot(x, y, type = "b")
              
            } else if(input$dpm_gen_input == "Geometric") {
              
              set.seed(1234)
              par(mfrow = c(1, 2))
              geom_plot <- rgeom(input$dpm_gen_s, input$pois_lambda)
              tab <- table(geom_plot)
              barplot(geom_plot, col = "blue")
              x <- 0:input$geom_end
              y <- dgeom(x, input$geom_p)
              plot(x, y, type = "b")
              
              
            } else if(input$dpm_gen_input == "Hypergeometric") {
              
              set.seed(1234)
              par(mfrow = c(1, 2))
              hyper_plot <- rhyper(input$dpm_gen_s, input$hyper_k, (input$hyper_ncap - input$hyper_k), input$hyper_j)
              tab <- table(hyper_plot)
              barplot(hyper_plot, col = "blue")
              x <- 0:input$hyper_n
              y <- dhyper(x, input$hyper_k, (input$hyper_ncap - input$hyper_k), input$hyper_n)
              plot(x, y, type = "b", xlab = "k", ylab = "PMF")
            }
          })
        })
        
        output$dpm_gen_prob <- renderPrint({
          
          input$dpm_gen_btn
          
          isolate({
            
            if(input$dpm_gen_input == "Bernoulli") {
              
              temp <- dbinom(input$bern_j, 1, input$bern_p)
              print(paste("Probability:", temp))
              
            } else if(input$dpm_gen_input == "Binomial") {
              
              print(paste("Probability :", dbinom(input$binom_j, input$binom_t, input$binom_p)))
              
            } else if(input$dpm_gen_input == "Multinomial") {
              
              temp1 <- c(-1)
              temp2 <- c(-1)
              if(input$mnom_k > 0) {
                for(i in 1:input$mnom_k) {
                  temp1 <- c(temp1, as.numeric(input[[paste0("p", as.character(i))]]))
                  temp2 <- c(temp2, as.numeric(input[[paste0("x", as.character(i))]]))
                }
              }
              temp1 <- temp1[-1]
              temp2 <- temp2[-1]
              sample_size <- sum(temp2)
              print(paste("Probability :", dmultinom(temp2, sample_size, temp1)))
              
            } else if(input$dpm_gen_input == "Poisson") {
              
              print(paste("Probability :", dpois(input$pois_j, input$pois_lambda)))
              
            } else if(input$dpm_gen_input == "Geometric") {
              
              print(paste("Probability :", dgeom(input$geom_j, input$geom_p)))
              
            } else if(input$dpm_gen_input == "Hypergeometric") {
              
              print(paste("Probability :", dhyper(input$hyper_j, input$hyper_k, (input$hyper_ncap - input$hyper_k), input$hyper_n )))
            }
          })
          
        })
        
        output$dpm_gen_tab <- DT::renderDataTable({
          
          input$dpm_gen_btn
          
          isolate({
            if(input$dpm_gen_input == "Bernoulli") {
              
              set.seed(1234)
              bern_tab <- rbinom(input$dpm_gen_s, 1, input$bern_p)
              DT::datatable(data.frame("j" = c("0", "1"), "No._of_observations" = c(table(bern_tab))))
              
            } else if(input$dpm_gen_input == "Binomial") {
              
              set.seed(1234)
              binom_tab <- dbinom(c(0:input$dpm_gen_s), input$binom_t, input$binom_p)
              DT::datatable(data.frame("j" = c(as.character(0:input$dpm_gen_s)), "Probability" = binom_tab))
              
            } else if(input$dpm_gen_input == "Multinomial") {
              
              set.seed(1234)
              temp1 <- c(-1)
              temp2 <- c(-1)
              if(input$mnom_k > 0) {
                for(i in 1:input$mnom_k) {
                  temp1 <- c(temp1, as.numeric(input[[paste0("p", as.character(i))]]))
                  temp2 <- c(temp2, as.numeric(input[[paste0("x", as.character(i))]]))
                }
              }
              
              temp1 <- temp1[-1]
              temp2 <- temp2[-1]
              sample_size <- sum(temp2)
              mnom_plot <- rmultinom(input$dpm_gen_s, sample_size, temp1)
              df = data.frame(mnom_plot)
              setattr(df, "row.names", as.character(temp1))
              DT::datatable(df)
              
            } else if(input$dpm_gen_input == "Poisson") {
              
              set.seed(1234)
              pois_tab <- dpois(c(0:input$dpm_gen_s), input$pois_lambda)
              DT::datatable(data.frame("j" = c(as.character(0:input$dpm_gen_s)), "Probability" = pois_tab))
              
            } else if(input$dpm_gen_input == "Geometric") {
              
              set.seed(1234)
              geom_tab <- dgeom(c(1:input$dpm_gen_s), input$geom_p)
              DT::datatable(data.frame("j" = c(as.character(1:input$dpm_gen_s)), "Probability" = geom_tab))
              
            } else if(input$dpm_gen_input == "Hypergeometric") {
              
              set.seed(1234)
              hyper_tab <- dhyper(c(0:input$hyper_n), input$hyper_k, (input$hyper_ncap - input$hyper_k), input$hyper_n)
              DT::datatable(data.frame("j" = c(as.character(0:input$hyper_n)), "Probability" = hyper_tab))
              
            }
          })
        })
        
        output$dpm_gen_exp <- renderPrint({
          
          input$dpm_gen_btn
          
          isolate({
            
            if(input$dpm_gen_input == "Bernoulli") {
              
              print(paste("Expectation :", input$bern_p))
              
            } else if(input$dpm_gen_input == "Binomial") {
              
              print(paste("Expectation :", (input$binom_t*input$binom_p)))
              
            } else if(input$dpm_gen_input == "Multinomial") {
              
              temp1 <- c(-1)
              temp2 <- c(-1)
              if(input$mnom_k > 0) {
                for(i in 1:input$mnom_k) {
                  temp1 <- c(temp1, as.numeric(input[[paste0("p", as.character(i))]]))
                  temp2 <- c(temp2, as.numeric(input[[paste0("x", as.character(i))]]))
                }
              }
              temp1 <- temp1[-1]
              temp2 <- temp2[-1]
              sample_size <- sum(temp2)
              
              for(i in 1:input$mnom_k) {
                print(paste0("Expectation (x", i, ") : ", as.character(sample_size*temp1[i])))
                br()
              }
            } else if(input$dpm_gen_input == "Poisson") {
              
              print(paste("Expectation :", input$pois_lambda))
              
            } else if(input$dpm_gen_input == "Geometric") {
              
              print(paste("Expectation :", (1/input$geom_p)))
              
            } else if(input$dpm_gen_input == "Hypergeometric") {
              
              print(paste("Expectation :", (input$hyper_n*(input$hyper_k/input$hyper_ncap))))
            }
          })
        })
        
        output$dpm_gen_var <- renderPrint({
          
          input$dpm_gen_btn
          
          isolate({
            if(input$dpm_gen_input == "Bernoulli") {
              
              print(paste("Variance :", input$bern_p*(1 - input$bern_p)))
              
            } else if(input$dpm_gen_input == "Binomial") {
              
              print(paste("Variance :", (input$binom_t*input$binom_p*(1 - input$binom_p))))
              
            } else if(input$dpm_gen_input == "Multinomial") {
              
              temp1 <- c(-1)
              temp2 <- c(-1)
              if(input$mnom_k > 0) {
                for(i in 1:input$mnom_k) {
                  temp1 <- c(temp1, as.numeric(input[[paste0("p", as.character(i))]]))
                  temp2 <- c(temp2, as.numeric(input[[paste0("x", as.character(i))]]))
                }
              }
              temp1 <- temp1[-1]
              temp2 <- temp2[-1]
              sample_size <- sum(temp2)
              
              for(i in 1:input$mnom_k) {
                print(paste0("Variance (x", i, ") : ", as.character(sample_size*temp1[i]*(1 - temp1[i]))))
                br()
              }
              
              for(i in 1:input$mnom_k) {
                for(j in 1:input$mnom_k) {
                  print(paste0("Co-Variance (x", i, ", x", j,") : ", as.character(-1*sample_size*temp1[i]*temp1[j])))
                }
              }
            } else if(input$dpm_gen_input == "Poisson") {
              
              print(paste("Variance :", input$pois_lambda))
              
            } else if(input$dpm_gen_input == "Geometric") {
              
              print(paste("Variance :", (1 - input$geom_p)/(input$geom_p^2)))
              
            } else if(input$dpm_gen_input == "Hypergeometric") {
              
              print(paste("Variance :", input$hyper_n*(input$hyper_k/input$hyper_ncap)*((input$hyper_ncap - input$hyper_k)/input$hyper_ncap)*((input$hyper_ncap - input$hyper_n)/(input$hyper_ncap - 1)) ))
            }
          })
        })
    })
    
    observeEvent(input$cpm_gen_btn, {
      
      output$cpm_gen_plot <- renderPlot({
        
        input$cpm_gen_btn
        
        isolate({
          if(input$cpm_gen_input == "Uniform") {
            
            set.seed(1234)
            unif_plot <- runif(input$cpm_gen_s, min = input$unif_a, max = input$unif_b)
            
            hist(unif_plot, 
                 freq = FALSE, 
                 xlab = "x",  
                 ylim = c(0, 1),
                 xlim = c(input$unif_a, input$unif_b),
                 density = 20,
                 main = "Uniform distribution")
            
            curve(dunif(x, min = input$unif_a, max = input$unif_b), 
                  from = input$unif_a, to = input$unif_b, 
                  n = input$unif_s, 
                  col = "darkblue", 
                  lwd = 2, 
                  add = TRUE, 
                  yaxt = "n",
                  ylab = "Probability")
            
          } else if(input$cpm_gen_input == "Normal") {
            
            set.seed(1234)
            x <- seq(-input$norm_i, input$norm_i, 0.01) 
            plot(x, dnorm(x, input$norm_mu, input$norm_sigma), type = "l", col = "red") 
            
          } else if(input$cpm_gen_input == "Exponential") {
            
            set.seed(1234)
            x <- seq(0, input$exp_i, 0.01) 
            plot(x, dexp(x, input$exp_lambda), type = "l", col = "green")
            
          } else if(input$cpm_gen_input == "Gamma") {
            
            set.seed(1234)
            x <- seq(0, input$gamma_i, 0.01)
            plot(x, dgamma(x, input$gamma_alpha, input$gamma_lambda), type = "l", col = "blue")
            
          } else if(input$cpm_gen_input == "Chi-Squared") {
            
            set.seed(1234)
            x <- seq(0, input$chisq_i, 0.01)
            plot(x, dchisq(x, input$chisq_k), type = "l", col = "red")
            
          }
        })
      })
      
      output$cpm_gen_prob <- renderPrint({
        
        input$cpm_gen_btn
        
        isolate({
          if(input$cpm_gen_input == "Uniform") {
            
            print(paste("Probability :", punif(input$unif_x, min = input$unif_a, max = input$unif_b)))
            
          } else if(input$cpm_gen_input == "Normal") {
            
            print(paste("Probability :", pnorm(input$norm_j, input$norm_mu, input$norm_sigma)))
            
          } else if(input$cpm_gen_input == "Exponential") {
            
            print(paste("Probability :", pexp(input$exp_j, input$exp_lambda)))
            
          } else if(input$cpm_gen_input == "Gamma") {
            
            print(paste("Probability :", pgamma(input$gamma_j, input$gamma_alpha, input$gamma_lambda)))
            
          } else if(input$cpm_gen_input == "Chi-Squared") {
            
            print(paste("Probability :", pchisq(input$chisq_j, input$chisq_k)))
  
          }
        })
      })
      
      output$cpm_gen_tab <- DT::renderDataTable({
        
        input$cpm_gen_btn
        
        isolate({
          if(input$cpm_gen_input == "Uniform") {
            
            set.seed(1234)
            unif_tab <- punif(c(input$unif_a:input$unif_b), min = input$unif_a, max = input$unif_b)
            DT::datatable(data.frame("x" = c(as.character(input$unif_a:input$unif_)), "Probability" = unif_tab))
            
          } else if(input$cpm_gen_input == "Normal") {
            
            set.seed(1234)
            norm_tab <- rnorm(input$cpm_gen_s, input$norm_mu, input$norm_sigma) 
            DT::datatable(data.frame(norm_tab))
            
          } else if(input$cpm_gen_input == "Exponential") {
            
            set.seed(1234)
            exp_tab <- rexp(input$cpm_gen_s, input$exp_lambda)
            DT::datatable(data.frame(exp_tab))
            
          } else if(input$cpm_gen_input == "Gamma") {
            
            set.seed(1234)
            gamma_tab <- rgamma(input$cpm_gen_s, input$gamma_alpha, input$gamma_lambda)
            DT::datatable(data.frame(gamma_tab))
            
          } else if(input$cpm_gen_input == "Chi-Squared") {
            
            set.seed(1234)
            chisq_tab <- rchisq(input$cpm_gen_s, input$chisq_k)
            DT::datatable(data.frame(chisq_tab))
            
          }
        })
      })
      
      output$cpm_gen_exp <- renderPrint({
        
        input$cpm_gen_btn
        
        isolate({
          if(input$cpm_gen_input == "Uniform") {
            
            print(paste("Expectation :", (input$unif_a + input$unif_b)/2))
            
          } else if(input$cpm_gen_input == "Normal") {
            
            print(paste("Expectation :", input$norm_mu))
            
          } else if(input$cpm_gen_input == "Exponential") {
            
            print(paste("Expectation :", 1/input$exp_lambda))
            
          } else if(input$cpm_gen_input == "Gamma") {
            
            print(paste("Expectation :", input$gamma_alpha/input$gamma_lambda))
            
          } else if(input$cpm_gen_input == "Chi-Squared") {
            
            print(paste("Expectation :", input$chisq_k ))
            
          }
        })
      })
      
      output$cpm_gen_var <- renderPrint({
        
        input$cpm_gen_btn
        
        isolate({
          if(input$cpm_gen_input == "Uniform") {
            
            print(paste("Variance :", ((input$unif_b - input$unif_a)^2)/12))
            
          } else if(input$cpm_gen_input == "Normal") {
            
            print(paste("Variance :", input$norm_sigma^2))
            
          } else if(input$cpm_gen_input == "Exponential") {
            
            print(paste("Variance :", 1/(input$exp_lambda^2)))
            
          } else if(input$cpm_gen_input == "Gamma") {
            
            print(paste("Variance :", input$gamma_alpha/(input$gamma_lambda^2)))
            
          } else if(input$cpm_gen_input == "Chi-Squared") {
            
            print(paste("Variance :", input$chisq_k*2))
            
          }
        })
      })
    })
    
    impCpmFile <- reactive({
      
      imp_file <- input$data_file$datapath
      if(is.null(imp_file)) {
        return()
      }
      data1 <- read.csv(file = imp_file, header = input$cpm_imp_header, sep = input$cpm_imp_sep)
      data1
    })
    
    impCpmURL <- reactive({
      
      imp_url <- input$url_input
      if(is.null(imp_url)) {
        return()
      }
      data2 <- read.csv(file = imp_url)
      data2
    })
    observeEvent(input$cpm_url_btn, {
        
        input$cpm_url_btn
        isolate(updateSelectInput(session, "cpm_imp_cols", choices = colnames(impCpmURL())))
        output$cpm_imp_tab <- DT::renderDataTable({
          input$cpm_url_btn
          isolate({
            DT::datatable(impCpmURL())
          })
        })
    })
    
    
    impCpmIB <- reactive({
      data3 <- get(input$cpm_ibds)
      data3
    })
    
    observe({
      switch(input$cpm_imp_source,
        file = { updateSelectInput(session, "cpm_imp_cols", choices = colnames(impCpmFile())) },
        url = { updateSelectInput(session, "cpm_imp_cols", choices = c("")) },
        inBuilt = { updateSelectInput(session, "cpm_imp_cols", choices = colnames(impCpmIB())) }
      )
    })
    
    observe({
      switch(input$cpm_imp_source,
             file = { output$cpm_imp_tab <- DT::renderDataTable({
                        DT::datatable(impCpmFile())
                      })
                  },
             url = {
               output$cpm_imp_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             },
             inBuilt = { 
                 output$cpm_imp_tab <- DT::renderDataTable({
                   DT::datatable(impCpmIB())
                 })
             }
      )
    })
    
    observeEvent(input$cpm_imp_btn, {
      
      output$cpm_imp_pred <- renderPrint({
      
       input$cpm_imp_btn
       isolate({
         temp_list <- switch(input$cpm_imp_source,
                             file = { impTemp_df <- impCpmFile()
                             impTemp_col <- na.omit(impTemp_df[, input$cpm_imp_cols])
                             new_list <- list(impTemp_df = impTemp_df, impTemp_col = impTemp_col)
                             },
                             url = { impTemp_df <- impCpmURL()
                             impTemp_col <- na.omit(impTemp_df[, input$cpm_imp_cols])
                             new_list <- list(impTemp_df = impTemp_df, impTemp_col = impTemp_col)
                             },
                             inBuilt = { impTemp_df <- impCpmIB()
                             impTemp_col <- na.omit(impTemp_df[, input$cpm_imp_cols])
                             new_list <- list(impTemp_df = impTemp_df, impTemp_col = impTemp_col)
                             }
         )
         
         if(input$cpm_gen_input == "Uniform") {
           
           if(is.character(temp_list$impTemp_col) | is.factor(temp_list$impTemp_col)) {
             print("Character values present! Please select another column")
           } else if(any(temp_list$impTemp_col < 0)) {
             print("Please select a column with Positive values")
           } else if(any(temp_list$impTemp_col%%1 != 0) | any(temp_list$impTemp_col%%1 == 0)) {
             print(paste("Predicted Value: ", mean(runif(input$cpm_imp_s, min = min(temp_list$impTemp_col), max = max(temp_list$impTemp_col))) ))
           } else {
             print("Please select a column with continuous values")
           }
         } else if(input$cpm_gen_input == "Normal") {
           
           if(is.character(temp_list$impTemp_col) | is.factor(temp_list$impTemp_col)) {
             print("Character values present! Please select another column")
           } else if(any(temp_list$impTemp_col%%1 != 0) | any(temp_list$impTemp_col%%1 == 0)) {
             print(paste("Predicted Value: ", mean(rnorm(input$cpm_imp_s, mean(temp_list$impTemp_col), sd(temp_list$impTemp_col))) ))
           } else {
             print("Please select a column with continuous values")
           }
         } else if(input$cpm_gen_input == "Exponential") {
           
           if(is.character(temp_list$impTemp_col) | is.factor(temp_list$impTemp_col)) {
             print("Character values present! Please select another column")
           } else if(any(temp_list$impTemp_col < 0)) {
             print("Please select a column with Positive values")
           } else if(any(temp_list$impTemp_col%%1 != 0) | any(temp_list$impTemp_col%%1 == 0)) {
             print(paste("Predicted Value: ", mean(rexp(input$cpm_imp_s, 1/mean(temp_list$impTemp_col))) ))
           } else {
             print("Please select a column with continuous values")
           }
         } else if(input$cpm_gen_input == "Gamma") {
           
           if(is.character(temp_list$impTemp_col) | is.factor(temp_list$impTemp_col)) {
             print("Character values present! Please select another column")
           } else if(any(temp_list$impTemp_col < 0)) {
             print("Please select a column with Positive values")
           } else if(any(temp_list$impTemp_col%%1 != 0) | any(temp_list$impTemp_col%%1 == 0)) {
             print(paste("Predicted Value: ", mean(rgamma(input$cpm_imp_s, input$imp_gamma_alpha, 1/mean(temp_list$impTemp_col))) ))
           } else {
             print("Please select a column with continuous values")
           }
         }
       })
      })
    })
    
    htMeanFile <- reactive({
      
      imp_file <- input$mean_fileInput$datapath
      if(is.null(imp_file)) {
        return()
      }
      data1 <- read.csv(file = imp_file, header = input$mean_header, sep = input$mean_sep)
      data1
    })
    
    htMeanURL <- reactive({
      imp_url <- input$mean_urlInput
      if(is.null(imp_url)) {
        return()
      }
      data2 <- read.csv(file = imp_url)
      data2
    })
  
    observeEvent(input$mean_url_btn, {
      
      input$mean_url_btn
      isolate(updateSelectInput(session, "mean_url_cols", choices = colnames(htMeanURL())))
      output$ht_mean_tab <- DT::renderDataTable({
        input$mean_url_btn
        isolate({
          DT::datatable(htMeanURL())
        })
      })
    })
    
    htMeanInBuilt <- reactive({
      data3 <- get(input$mean_inBuiltInput)
      data3
    })
    
    
    htMeanYfin <- reactive({
      tickerName <- input$mean_tickerInput
      
      startDate <- Sys.Date() - 1*365
      endDate <- Sys.Date()
      
      stockData <- data.frame(pdfetch_YAHOO(identifiers = tickerName, fields = c("open", "high", "low", "volume", "close"), 
                                            from = as.Date(startDate), to = endDate, interval = input$mean_freqInput))
      
      names(stockData)[names(stockData) == paste0(tickerName, ".open")] <- "Open"
      names(stockData)[names(stockData) == paste0(tickerName, ".high")] <- "High"
      names(stockData)[names(stockData) == paste0(tickerName, ".low")] <- "Low"
      names(stockData)[names(stockData) == paste0(tickerName, ".volume")] <- "Volume"
      names(stockData)[names(stockData) == paste0(tickerName, ".close")] <- "Close"
      
      stockData
    })
    
    observeEvent(input$mean_yfin_btn, {
      
      input$mean_yfin_btn
      isolate(updateSelectInput(session, "mean_yfin_cols", choices = colnames(htMeanYfin())))
      output$ht_mean_tab <- DT::renderDataTable({
        input$mean_yfin_btn
        isolate({
          DT::datatable(htMeanYfin())
        })
      })
    })
    
    output$mean_ref_tab <- renderUI({
      tags$iframe(style = "height:525px; width:100%", src = "Z-table.pdf")
    })
    
    observeEvent(input$mean_btn, {
      
      output$ht_mean_plot <- renderPlot({
        
        input$mean_btn
        isolate({
          
          mean_list <- switch(input$ht_mean_source,
                              
                              mean_input = {
                                new_list <- list(mean = input$mean_mu, sd = input$mean_sigma, alpha = input$mean_alpha)
                              },
                              mean_file = { 
                                tempDF <- htMeanFile()
                                tempCol <- na.omit(tempDF[, input$mean_file_cols])
                                new_list <- list(mean = mean(tempCol), sd = sd(tempCol), alpha = input$mean_alpha)
                              },
                              mean_url = { 
                                tempDF <- htMeanURL()
                                tempCol <- na.omit(tempDF[, input$mean_url_cols])
                                new_list <- list(mean = mean(tempCol), sd = sd(tempCol), alpha = input$mean_alpha)
                              },
                              mean_inBuilt = { 
                                tempDF <- htMeanInBuilt()
                                tempCol <- na.omit(tempDF[, input$mean_ibds_cols])
                                temp <- list(mean = mean(tempCol), sd = sd(tempCol), alpha = input$mean_alpha)
                              },
                              mean_yfin = {
                                tempDF <- htMeanYfin()
                                tempCol <- na.omit(tempDF[, input$mean_yfin_cols])
                                temp <- list(mean = mean(tempCol), sd = sd(tempCol), alpha = input$mean_alpha)
                              })
          
            switch(input$mean_rb, 
                   meanInput_left = {
                     
                     mean <- mean_list$mean 
                     sd <- mean_list$sd
                     lb <- -(mean + (4*sd))
                     ub <- qnorm(mean_list$alpha)
                     
                     x <- seq(-(mean + (4*sd)), (mean + (4*sd)), length = 100)
                     hx <- dnorm(x, mean, sd)
                     
                     plot(x, hx, type = "l", xlab = "", ylab = "",
                          main = "Normal Distribution", axes = FALSE)
                     
                     i <- x >= lb & x <= ub
                     polygon(c(lb, x[i], ub), c(0, hx[i], 0), col = "red")
                     
                     area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
                     result <- paste("Mean = ", mean, ", SD = ", sd, ", Area",
                                     signif(area, digits = 3))
                     mtext(result, 3)
                     axis(1, 
                          at = c(-(mean + (4*sd)), 
                                 (mean + (4*sd)),
                                 qnorm(mean_list$alpha)),
                          labels = c(signif(-(mean + (4*sd)), digits = 1), 
                                     signif((mean + (4*sd)), digits = 1), 
                                     qnorm(mean_list$alpha)
                          ))
                   },
                   meanInput_right = {
                     
                     mean <- mean_list$mean 
                     sd <- mean_list$sd
                     lb <- qnorm(1 - mean_list$alpha)
                     ub <- (mean + (4*sd))
                     
                     x <- seq(-(mean + (4*sd)), (mean + (4*sd)), length = 100)
                     hx <- dnorm(x, mean, sd)
                     
                     plot(x, hx, type = "l", xlab = "", ylab = "",
                          main = "Normal Distribution", axes = FALSE)
                     
                     i <- x >= lb & x <= ub
                     polygon(c(lb, x[i], ub), c(0, hx[i], 0), col = "red")
                     
                     area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
                     result <- paste("Mean = ", mean, ", SD = ", sd, ", Area",
                                     signif(area, digits = 3))
                     mtext(result, 3)
                     axis(1, 
                          at = c(-(mean + (4*sd)), 
                                 (mean + (4*sd)),
                                 qnorm(1 - mean_list$alpha)),
                          labels = c(signif(-(mean + (4*sd)), digits = 1), 
                                     signif((mean + (4*sd)), digits = 1), 
                                     qnorm(1 - mean_list$alpha)
                          ))
                     
                   },
                   meanInput_two = {
                     
                     mean <- mean_list$mean 
                     sd <- mean_list$sd
                     lb1 <- -(mean + (4*sd))
                     ub1 <- qnorm(mean_list$alpha/2)
                     lb2 <- qnorm(1 - (mean_list$alpha/2))
                     ub2 <- (mean + (4*sd))
                     
                     x <- seq(-(mean + (4*sd)), (mean + (4*sd)), length = 100)
                     hx <- dnorm(x, mean, sd)
                     
                     plot(x, hx, type = "l", xlab = "", ylab = "",
                          main = "Normal Distribution", axes = FALSE)
                     
                     i1 <- x >= lb1 & x <= ub1
                     i2 <- x >= lb2 & x <= ub2
                     polygon(c(lb1, x[i1], ub1), c(0, hx[i1], 0), col = "red")
                     polygon(c(lb2, x[i2], ub2), c(0, hx[i2], 0), col = "red")
                     
                     area <- (pnorm(ub1, mean, sd) - pnorm(lb1, mean, sd)) + (pnorm(ub2, mean, sd) - pnorm(lb2, mean, sd))
                     result <- paste("Mean = ", mean, ", SD = ", sd, ", Area",
                                     signif(area, digits = 3))
                     mtext(result, 3)
                     axis(1, 
                          at = c(-(mean + (4*sd)), 
                                 (mean + (4*sd)),
                                 qnorm(1 - (mean_list$alpha/2)),
                                 qnorm(mean_list$alpha/2)),
                          labels = c(signif(-(mean + (4*sd)), digits = 1),
                                     signif((mean + (4*sd)), digits = 1), 
                                     qnorm(1 - (mean_list$alpha/2)),
                                     qnorm(mean_list$alpha/2))
                          )
                   })
          
        })
      })
      
      output$ht_mean_decision <- renderPrint({
        
        input$mean_btn
        isolate({
          
          decisionList <- switch(input$ht_mean_source,
                 
                 mean_input = {
                   temp <- list(mean = input$mean_mu, sd = input$mean_sigma, xbar = input$mean_xbar, n = input$mean_n, alpha = input$mean_alpha)
                 },
                 mean_file = { 
                   tempDF <- htMeanFile()
                   tempCol <- na.omit(tempDF[, input$mean_file_cols])
                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                 },
                 mean_url = { 
                   tempDF <- htMeanURL()
                   tempCol <- na.omit(tempDF[, input$mean_url_cols])
                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                 },
                 mean_inBuilt = { 
                   tempDF <- htMeanInBuilt()
                   tempCol <- na.omit(tempDF[, input$mean_ibds_cols])
                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                 },
                 mean_yfin = {
                   tempDF <- htMeanYfin()
                   tempCol <- na.omit(tempDF[, input$mean_yfin_cols])
                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                 })
          
          switch (input$mean_rb,
                  meanInput_left = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (decisionList$xbar - decisionList$mean)/(decisionList$sd/sqrt(decisionList$n))
                      c_value <- qnorm(decisionList$alpha)

                      if(test_value < c_value) {
                        print("Reject Ho")
                      } else {
                        print("Accept Ho")
                      }
                      
                    } else {
                      
                      t_test <- t.test(decisionList$colVector, mu = decisionList$mean, alternative = 'less', conf.level = (1 - decisionList$alpha))
                      if(t_test$p.value < decisionList$alpha) {
                        print("Reject Ho")
                      } else {
                        print("Accept Ho")
                      }
                    }
                  },
                  meanInput_right = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (decisionList$xbar - decisionList$mean)/(decisionList$sd/sqrt(decisionList$n))
                      c_value <- qnorm(1 - decisionList$alpha) 
                      
                      if(test_value < c_value) {
                        print("Accept Ho")
                      } else {
                        print("Reject Ho")
                      }
                      
                    } else {
                      
                      t_test <- t.test(decisionList$colVector, mu = decisionList$mean, alternative = 'greater', conf.level = (1 - decisionList$alpha))
                      if(t_test$p.value < decisionList$alpha) {
                        print("Reject Ho")
                      } else {
                        print("Accept Ho")
                      }
                    }
                  },
                  meanInput_two = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (decisionList$xbar - decisionList$mean)/(decisionList$sd/sqrt(decisionList$n))
                      c_value <- qnorm(1 - decisionList$alpha/2) 
                      
                      if(abs(test_value) >= abs(c_value)) {
                        print("Reject Ho")
                      } else {
                        print("Accept Ho")
                      }
                      
                    } else {
                      
                      t_test <- t.test(decisionList$colVector, mu = decisionList$mean, alternative = 'two.sided', conf.level = (1 - decisionList$alpha))
                      if(t_test$p.value < decisionList$alpha) {
                        print("Reject Ho")
                      } else {
                        print("Accept Ho")
                      }
                    }
                  }
          )
        })
      })
      
      output$ht_mean_result <- renderPrint({
        
        input$mean_btn
        isolate({
          
          resultList <- switch(input$ht_mean_source,
                                 
                                 mean_input = {
                                   temp <- list(mean = input$mean_mu, sd = input$mean_sigma, xbar = input$mean_xbar, n = input$mean_n, alpha = input$mean_alpha)
                                 },
                                 mean_file = { 
                                   tempDF <- htMeanFile()
                                   tempCol <- na.omit(tempDF[, input$mean_file_cols])
                                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                                 },
                                 mean_url = { 
                                   tempDF <- htMeanURL()
                                   tempCol <- na.omit(tempDF[, input$mean_url_cols])
                                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                                 },
                                 mean_inBuilt = { 
                                   tempDF <- htMeanInBuilt()
                                   tempCol <- na.omit(tempDF[, input$mean_ibds_cols])
                                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                                 },
                                 mean_yfin = {
                                   tempDF <- htMeanYfin()
                                   tempCol <- na.omit(tempDF[, input$mean_yfin_cols])
                                   temp <- list(mean = input$mean_mu, colVector = tempCol, alpha = input$mean_alpha)
                                 })
          
          switch (input$mean_rb,
                  meanInput_left = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (resultList$xbar - resultList$mean)/(resultList$sd/sqrt(resultList$n))
                      c_value <- qnorm(resultList$alpha)
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                      
                    } else {
                      
                      t_test <- t.test(resultList$colVector, mu = resultList$mean, alternative = "less", conf.level = (1 - resultList$alpha))
                      test_value <- t_test$statistic[["t"]]
                      c_value <- qnorm(resultList$alpha)
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                    }
                  },
                  meanInput_right = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (resultList$xbar - resultList$mean)/(resultList$sd/sqrt(resultList$n))
                      c_value <- qnorm(1 - resultList$alpha) 
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                      
                    } else {
                      
                      t_test <- t.test(resultList$colVector, mu = resultList$mean, alternative = "greater", conf.level = (1 - resultList$alpha))
                      test_value <- t_test$statistic[["t"]]
                      c_value <- qnorm(1 - resultList$alpha)
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                    }
                  },
                  meanInput_two = {
                    
                    if(input$ht_mean_source == "mean_input") {
                      
                      test_value <- (resultList$xbar - resultList$mean)/(resultList$sd/sqrt(resultList$n))
                      c_value <- abs(qnorm(1 - resultList$alpha/2))
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                      
                    } else {
                      
                      t_test <- t.test(resultList$colVector, mu = resultList$mean, alternative = "two.sided", conf.level = (1 - resultList$alpha))
                      test_value <- t_test$statistic[["t"]]
                      c_value <- abs(qnorm(1 - resultList$alpha/2))
                      
                      print(paste("Test - Value :", test_value))
                      br()
                      print(paste("Critical - Value :", c_value))
                    }
                  }
          )
        })
      })
    })
    
    observe({
      switch(input$ht_mean_source,
             mean_file = { updateSelectInput(session, "mean_file_cols", choices = colnames(htMeanFile())) },
             mean_url = { updateSelectInput(session, "mean_url_cols", choices = c("")) },
             mean_inBuilt = { updateSelectInput(session, "mean_ibds_cols", choices = colnames(htMeanInBuilt())) },
             mean_yfin = { updateSelectInput(session, "mean_yfin_cols", choices = c("")) }
      )
    })
    
    observe({
      switch(input$ht_mean_source,
             
             mean_input = {
               output$ht_mean_tab_ui <- renderText({
                 print("No Table to display")
               })
               output$ht_mean_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             },
             mean_file = { 
               output$ht_mean_tab_ui <- renderText({
                 print("")
               })
               output$ht_mean_tab <- DT::renderDataTable({
                 DT::datatable(htMeanFile())
               })
             },
             mean_url = {
               output$ht_mean_tab_ui <- renderText({
                 print("")
               })
               output$ht_mean_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             },
             mean_inBuilt = { 
               output$ht_mean_tab_ui <- renderText({
                 print("")
               })
               output$ht_mean_tab <- DT::renderDataTable({
                 DT::datatable(htMeanInBuilt())
               })
             },
             mean_yfin = {
               output$ht_mean_tab_ui <- renderText({
                 print("")
               })
               output$ht_mean_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             }
      )
    })
    
    glmLinearFile <- reactive({
      
      imp_file <- input$lin_fileInput$datapath
      if(is.null(imp_file)) {
        return()
      }
      data1 <- read.csv(file = imp_file, header = input$lin_header, sep = input$lin_sep)
      data1
    })
    
    glmLinearURL <- reactive({
      
      imp_url <- input$lin_urlInput
      if(is.null(imp_url)) {
        return()
      }
      data2 <- read.csv(file = imp_url)
      data2
    })
    
    observeEvent(input$lin_url_btn, {
      
      input$lin_url_btn
      isolate({
        updateSelectInput(session, "glm_lin_targetCol", choices = colnames(glmLinearURL()))
        observe({
          updateSelectInput(session, "glm_lin_independentCol", choices = colnames(glmLinearURL())[-which(colnames(glmLinearURL()) == input$glm_lin_targetCol)])
        })
      })
      
      output$glm_lin_tab <- DT::renderDataTable({
        input$lin_url_btn
        isolate({
          DT::datatable(glmLinearURL())
        })
      })
    })
    
    glmLinearInBuilt <- reactive({
      data3 <- get(input$lin_inBuiltInput)
      data3
    })
    
    
    glmLinearYfin <- reactive({
      tickerName <- input$lin_tickerInput
      
      startDate <- Sys.Date() - 1*365
      endDate <- Sys.Date()
      
      stockData <- data.frame(pdfetch_YAHOO(identifiers = tickerName, fields = c("open", "high", "low", "volume", "close"), 
                                            from = as.Date(startDate), to = endDate, interval = input$lin_freqInput))
      
      names(stockData)[names(stockData) == paste0(tickerName, ".open")] <- "Open"
      names(stockData)[names(stockData) == paste0(tickerName, ".high")] <- "High"
      names(stockData)[names(stockData) == paste0(tickerName, ".low")] <- "Low"
      names(stockData)[names(stockData) == paste0(tickerName, ".volume")] <- "Volume"
      names(stockData)[names(stockData) == paste0(tickerName, ".close")] <- "Close"
      
      stockData
    })
    
    observeEvent(input$lin_yfin_btn, {
      
      input$lin_yfin_btn
      isolate({
        updateSelectInput(session, "glm_lin_targetCol", choices = colnames(glmLinearYfin()))
        observe({
          updateSelectInput(session, "glm_lin_independentCol", choices = colnames(glmLinearYfin())[-which(colnames(glmLinearYfin()) == input$glm_lin_targetCol)])
        })
      })
      
      output$glm_lin_tab <- DT::renderDataTable({
        input$lin_yfin_btn
        isolate({
          DT::datatable(glmLinearYfin())
        })
      })
    })
    
    output$linkFunc_ui <- renderUI({
      if(length(input$glm_lin_independentCol) > 0) {
        lapply(1:length(input$glm_lin_independentCol) , function(i) { 
          strong(numericInput(as.character(paste0("col_x", i)), paste0(input$glm_lin_independentCol[i]), value = as.numeric(i)),
                 br())
        })
      }
    })
    
    observeEvent(input$glm_lin_btn,{
      
      output$glm_lin_pred <- renderPrint({
        
        input$glm_lin_btn
        isolate({
          
          predictList <- switch(input$glm_lin_source,
            lin_file = {
              datasetDF <- glmLinearFile()
              targetCol <- datasetDF[, input$glm_lin_targetCol]
              independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
              newList <- list(tarData = targetCol, indData = independentCols)
            },
            lin_url = {
              datasetDF <- glmLinearURL()
              targetCol <- datasetDF[, input$glm_lin_targetCol]
              independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
              newList <- list(tarData = targetCol, indData = independentCols)
            },
            lin_inBuilt = {
              datasetDF <- glmLinearInBuilt()
              targetCol <- datasetDF[, input$glm_lin_targetCol]
              independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
              newList <- list(tarData = targetCol, indData = independentCols)
            },
            lin_yfin = {
              datasetDF <- glmLinearYfin()
              targetCol <- datasetDF[, input$glm_lin_targetCol]
              independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
              newList <- list(tarData = targetCol, indData = independentCols)
            }
          )
          
          switch (input$glm_lin_type,
                  
            glm_lin_func = {
              
              colNameVector <- c("-1")
              tempDF <- data.frame(rep(0, length(predictList$tarData)))
              
              for(i in 1:length(predictList$indData)) {
                colNameVector <- c(colNameVector, paste0("xcol", i))
                assign(paste0("xcol", i), predictList$indData[, i])
                tempDF <- cbind(tempDF, get(paste0("xcol", i)))
              }
              ycol <- as.vector(predictList$tarData)
              colNameVector <- colNameVector[-1]
              tempDF <- cbind(tempDF, ycol)
              tempDF <- tempDF[, -1]

              names(tempDF) <- c(colNameVector, "ycol")

              tempFit <- glm(as.formula(paste("ycol ~", paste(colNameVector, collapse = "+"))), data = tempDF,family = "gaussian")
              print(summary(tempFit))

              coefVector <- c(1)
              if(length(input$glm_lin_independentCol) > 0) {
                for(i in 1:length(predictList$indData)) {
                  coefVector <- c(coefVector, as.numeric(input[[paste0("col_x", as.character(i))]]))
                }
              }

              estVector <- c(-1)
              for(i in 1:sum(length(colNameVector) + 1)) {
                if(summary(tempFit)$coef[i,4] <= input$lin_alpha) {
                  estVector <- c(estVector, summary(tempFit)$coef[i, 1])
                } else {
                  estVector <- c(estVector, 0)
                }
              }

              estVector <- estVector[-1]
              predictedValue <- sum(estVector*coefVector)
              print(paste("Predicted Value:", predictedValue))
            
            },
            glm_lin_rmse = {
              
              colNameVector <- c("-1")
              tempDF <- data.frame(rep(0, length(predictList$tarData)))
              
              for(i in 1:length(predictList$indData)) {
                colNameVector <- c(colNameVector, paste0("xcol", i))
                assign(paste0("xcol", i), predictList$indData[, i])
                tempDF <- cbind(tempDF, get(paste0("xcol", i)))
              }
              ycol <- as.vector(predictList$tarData)
              colNameVector <- colNameVector[-1]
              tempDF <- cbind(tempDF, ycol)
              tempDF <- tempDF[, -1]
              
              names(tempDF) <- c(colNameVector, "ycol")
              
              if(input$glm_lin_rb == "modelInput_full") {
                
                rmse_avg <- 0
                
                for(i in 1:input$glm_lin_mc) {
                  n <- nrow(tempDF)
                  set.seed(1798)
                  indexes <- sample(n , n*(input$glm_lin_dataSplit/100))
                  trainSet <- tempDF[indexes,]
                  testSet <- tempDF[-indexes,]
                  
                  full_model <- glm(trainSet$ycol ~. , data = trainSet, family = "gaussian")
                  predFull <- predict(full_model, testSet[1:(length(testSet) - 1)])
                  actualVal <- testSet[, "ycol"]
                  valueDF <- data.frame("Predicted_Value" = predFull, "Actual_Value" = actualVal)
                  rmse_full <- sqrt(sum((predFull - actualVal)^2)/nrow(testSet))
                  
                  rmse_avg <- rmse_avg + rmse_full
                }
                
                print(valueDF)
                print(paste("RMSE Full Model :", rmse_avg/input$glm_lin_mc))
                
              } else if(input$glm_lin_rb == "modelInput_red") {
                
                rmse_avg <- 0
                
                for(i in 1:input$glm_lin_mc) {
                  n <- nrow(tempDF)
                  set.seed(1798)
                  indexes <- sample(n , n*(input$glm_lin_dataSplit/100))
                  trainSet <- tempDF[indexes,]
                  testSet <- tempDF[-indexes,]
                  
                  full_model <- glm(trainSet$ycol ~. , data = trainSet, family = "gaussian")
                  red_model <- stepAIC(full_model, trace = FALSE)
                  predRed <- predict(red_model, testSet[1:(length(testSet) - 1)])
                  actualVal <- testSet[, "ycol"]
                  valueDF <- data.frame("Predicted_Value" = predRed, "Actual_Value" = actualVal)
                  rmse_red <- sqrt(sum((predRed - actualVal)^2)/nrow(testSet))
                  
                  rmse_avg <- rmse_avg + rmse_red
                }
                
                print(valueDF)
                print(paste("RMSE Reduced Model :", rmse_avg/input$glm_lin_mc))
              }
            }
          )
        })
      })
      
      output$glm_lin_plotMessage <- renderText({
        
        input$glm_lin_btn
        isolate({
          switch (input$glm_lin_type,
                  
                  glm_lin_func = {
                    print("No Plot to display")
                  },
                  glm_lin_rmse = {
                    print("")
                  }
          )
        })
      })
      
      output$glm_lin_plot <- renderPlot({

        input$glm_lin_btn
        isolate({
          plotList <- switch(input$glm_lin_source,
                                lin_file = {
                                  datasetDF <- glmLinearFile()
                                  targetCol <- datasetDF[, input$glm_lin_targetCol]
                                  independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
                                  newList <- list(tarData = targetCol, indData = independentCols)
                                },
                                lin_url = {
                                  datasetDF <- glmLinearURL()
                                  targetCol <- datasetDF[, input$glm_lin_targetCol]
                                  independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
                                  newList <- list(tarData = targetCol, indData = independentCols)
                                },
                                lin_inBuilt = {
                                  datasetDF <- glmLinearInBuilt()
                                  targetCol <- datasetDF[, input$glm_lin_targetCol]
                                  independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
                                  newList <- list(tarData = targetCol, indData = independentCols)
                                },
                                lin_yfin = {
                                  datasetDF <- glmLinearYfin()
                                  targetCol <- datasetDF[, input$glm_lin_targetCol]
                                  independentCols <- data.frame(datasetDF[, input$glm_lin_independentCol])
                                  newList <- list(tarData = targetCol, indData = independentCols)
                                }
          )

          switch (input$glm_lin_type,
                  
                  glm_lin_func = {
                    
                    par(bg = "#ecf0f5")
                    plot.new()
                  },
                  glm_lin_rmse = {
                    
                    colNameVector <- c("-1")
                    tempDF <- data.frame(rep(0, length(plotList$tarData)))
                    
                    for(i in 1:length(plotList$indData)) {
                      colNameVector <- c(colNameVector, paste0("xcol", i))
                      assign(paste0("xcol", i), plotList$indData[, i])
                      tempDF <- cbind(tempDF, get(paste0("xcol", i)))
                    }
                    ycol <- as.vector(plotList$tarData)
                    colNameVector <- colNameVector[-1]
                    tempDF <- cbind(tempDF, ycol)
                    tempDF <- tempDF[, -1]
                    
                    names(tempDF) <- c(colNameVector, "ycol")
                    
                    if(input$glm_lin_rb == "modelInput_full") {
                      
                      rmse_avg <- 0
                      
                      for(i in 1:input$glm_lin_mc) {
                        n <- nrow(tempDF)
                        set.seed(1798)
                        indexes <- sample(n , n*(input$glm_lin_dataSplit/100))
                        trainSet <- tempDF[indexes,]
                        testSet <- tempDF[-indexes,]
                        
                        full_model <- glm(trainSet$ycol ~. , data = trainSet, family = "gaussian")
                        predFull <- predict(full_model, testSet[1:(length(testSet) - 1)])
                        actualVal <- testSet[, "ycol"]
                      }
                      
                      plot(actualVal, type = "o", col = "red", xlab = "Observations", ylab = input$glm_lin_targetCol, main = "Full Model")
                      lines(predFull, type = "o", col = "blue")
                      legend("topright", legend = c("Actual Value", "Predicted Value"),
                             col = c("red", "blue"), lty = 1:2, cex = 0.6)
                      
                    } else if(input$glm_lin_rb == "modelInput_red") {
                      
                      rmse_avg <- 0
                      
                      for(i in 1:input$glm_lin_mc) {
                        n <- nrow(tempDF)
                        set.seed(1798)
                        indexes <- sample(n , n*(input$glm_lin_dataSplit/100))
                        trainSet <- tempDF[indexes,]
                        testSet <- tempDF[-indexes,]
                        
                        full_model <- glm(trainSet$ycol ~. , data = trainSet, family = "gaussian")
                        red_model <- stepAIC(full_model, trace = FALSE)
                        predRed <- predict(red_model, testSet[1:(length(testSet) - 1)])
                        actualVal <- testSet[, "ycol"]
                      }
                      
                      plot(actualVal, type = "o", col = "red", xlab = "Observations", ylab = input$glm_lin_targetCol, main = "Full Model")
                      lines(predRed, type = "o", col = "blue")
                      legend("topright", legend = c("Actual Value", "Predicted Value"),
                             col = c("red", "blue"), lty = 1:2, cex = 0.6)
                    }
                  }
          )
        })
      })
    })
    
    observe({
      switch(input$glm_lin_source,
             lin_file = { updateSelectInput(session, "glm_lin_targetCol", choices = colnames(glmLinearFile()))
                          observe({
                            updateSelectInput(session, "glm_lin_independentCol", choices = colnames(glmLinearFile())[-which(colnames(glmLinearFile()) == input$glm_lin_targetCol)])
                          }) 
                        },
             lin_url = { updateSelectInput(session, "glm_lin_targetCol", choices = c(""))
                         updateSelectInput(session, "glm_lin_independentCol", choices = c("")) 
                       },
             lin_inBuilt = { updateSelectInput(session, "glm_lin_targetCol", choices = colnames(glmLinearInBuilt()))
                             observe({
                               updateSelectInput(session, "glm_lin_independentCol", choices = colnames(glmLinearInBuilt())[-which(colnames(glmLinearInBuilt()) == input$glm_lin_targetCol)])
                             }) 
                           },
             lin_yfin = { updateSelectInput(session, "glm_lin_targetCol", choices = c(""))
                          updateSelectInput(session, "glm_lin_independentCol", choices = c("")) 
                        }
      )
    })

    observe({
      switch(input$glm_lin_source,
             
             lin_file = { 
               output$glm_lin_tab <- DT::renderDataTable({
                 DT::datatable(glmLinearFile())
               })
             },
             lin_url = {
               output$glm_lin_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             },
             lin_inBuilt = { 
               output$glm_lin_tab <- DT::renderDataTable({
                 DT::datatable(glmLinearInBuilt())
               })
             },
             lin_yfin = {
               output$glm_lin_tab <- DT::renderDataTable({
                 DT::datatable(data.frame())
               })
             }
      )
    })
  
  }
)