# install.packages("selenium-r")
library(selenium)
library(rvest)
library(dplyr)
library(readr)
library(purrr)

# Configurações para ambiente CI
max_retries <- as.numeric(Sys.getenv("RETRY_COUNT", 3))
wait_time <- as.numeric(Sys.getenv("WAIT_TIME", 10))

# Configurar selenium para modo não interativo
options(selenium.interactive = FALSE)

run_selenium_automation <- function() {
  tryCatch({
    message("Configurando ambiente Selenium...")
    
    # Verificar se o servidor está rodando
    system("curl -f http://localhost:4444/status", intern = TRUE)
    
    # Usar servidor já existente em localhost:4444
    message("Conectando ao Selenium Server...")
    session <- SeleniumSession$new(
      browser = "firefox",
      host = "localhost",
      port = 4444L
    )
    
    Sys.sleep(5)
    
    # Abrir o site
    message("Navegando para o site...")
    session$navigate("https://web-server-flask.onrender.com/")
    Sys.sleep(wait_time)
    
    # Clica no botão login
    message("Fazendo login...")
    session$
      find_element("xpath", "//a[contains(text(), 'Login')]")$
      click()
    Sys.sleep(5)
    
    # Campo de e-mail
    session$
      find_element("css selector", "input[name='email']")$
      send_keys("team@gmail.com")
    Sys.sleep(3)
    
    # Campo de senha
    session$
      find_element("css selector", "input[name='password']")$
      send_keys("uF748978VAm4%88L")
    
    # Clica no botão de login pra entrar no site
    session$
      find_element("xpath", "//button[contains(text(), 'Login')]")$
      click()
    Sys.sleep(5)
    
    ########################################################################
    # SharePoint
    message("Extraindo dados do SharePoint...")
    session$
      find_element("xpath", "//a[contains(text(), 'SharePoint')]")$
      click()
    Sys.sleep(5)
    
    sharepoint <- list()
    num_paginas <- 3
    
    for (i in 1:num_paginas) {
      message(paste("SharePoint - Página", i))
      
      # Captura o HTML da página atual
      page_html <- session$get_page_source()
      html <- read_html(page_html)
      
      # Extrai as linhas da tabela
      linhas <- html %>%
        html_element("#table-body") %>%
        html_elements("tr")
      
      if (length(linhas) == 0) {
        message("Nenhuma linha encontrada na tabela SharePoint")
        break
      }
      
      # Extrai as células (td)
      dados <- map(linhas, function(tr) {
        tr %>% html_elements("td") %>% html_text(trim = TRUE)
      })
      
      # Filtra linhas vazias
      dados <- dados[map_int(dados, length) > 0]
      
      if (length(dados) > 0) {
        # Converte para data.frame
        sharepoint_pag <- map_dfr(dados, function(x) {
          as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
        })
        
        sharepoint[[i]] <- sharepoint_pag
        
        # Tenta clicar em "Próxima", se existir
        next_btn <- try(
          session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
          silent = TRUE
        )
        
        if (!inherits(next_btn, "try-error")) {
          session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
          Sys.sleep(2)
          next_btn$click()
          Sys.sleep(3) 
        } else {
          message("Botão 'Próxima' não encontrado, saindo do loop SharePoint")
          break
        }
      } else {
        message("Nenhum dado extraído do SharePoint")
        break
      }
    }
    
    if (length(sharepoint) > 0) {
      tabela_sharepoint <- bind_rows(sharepoint)
      
      if(ncol(tabela_sharepoint) >= 8) {
        colnames(tabela_sharepoint) <- c("Codigo_SAP", "Codigo_Litigio", "Data_Abertura",
                                  "Material", "Nota_Fiscal", "Data_Litigio",
                                  "Status", "Fornecedor")[1:ncol(tabela_sharepoint)]
        
        write_csv(tabela_sharepoint, "tabela_sharepoint.csv")
        message("Dados do SharePoint salvos com sucesso!")
      }
    }
    
    ########################################################################
    # SAP
    message("Extraindo dados do SAP...")
    session$execute_script("window.scrollTo(0, -document.body.scrollHeight);")
    Sys.sleep(2)
    
    session$
      find_element("xpath", "//a[contains(text(), 'Sap')]")$
      click()
    Sys.sleep(3) 
    
    sap <- list()
    num_paginas <- 3
    
    for (i in 1:num_paginas) {
      message(paste("SAP - Página", i))
      
      page_html <- session$get_page_source()
      html <- read_html(page_html)
      
      linhas <- html %>%
        html_element("#litigios-body") %>%
        html_elements("tr")
      
      if (length(linhas) == 0) {
        message("Nenhuma linha encontrada na tabela SAP")
        break
      }
      
      dados <- map(linhas, function(tr) {
        tr %>% html_elements("td") %>% html_text(trim = TRUE)
      })
      
      dados <- dados[map_int(dados, length) > 0]
      
      if (length(dados) > 0) {
        sap_pag <- map_dfr(dados, function(x) {
          as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
        })
        
        sap[[i]] <- sap_pag
        
        next_btn <- try(
          session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
          silent = TRUE
        )
        
        if (!inherits(next_btn, "try-error")) {
          session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
          Sys.sleep(2)
          next_btn$click()
          Sys.sleep(3)
        } else {
          message("Botão 'Próxima' não encontrado, saindo do loop SAP")
          break
        }
      } else {
        message("Nenhum dado extraído do SAP")
        break
      }
    }
    
    if (length(sap) > 0) {
      tabela_sap <- bind_rows(sap)
      
      if(ncol(tabela_sap) >= 20) {
        colnames(tabela_sap) <- c("Codigo_Litigio", "Item_Fatura", "Status", "Empresa", "Centro",
                                  "Grupo_Compradores", "Data_Criacao", "Fornecedor", "Nome_Fornecedor",
                                  "Documento_Compras", "Item", "Material", "Descricao_Material",
                                  "Referencia", "Quantidade", "Preco_Contrato", "Preco_Fatura",
                                  "Valor_Diferenca", "Montante", "Item_Contrato")[1:ncol(tabela_sap)]
        
        write_csv(tabela_sap, "tabela_sap.csv")
        message("Dados do SAP salvos com sucesso!")
      }
    }
    
    ########################################################################
    # NEOGRID
    message("Extraindo dados da Neogrid...")
    session$execute_script("window.scrollTo(0, -document.body.scrollHeight);")
    Sys.sleep(2)
    
    session$
      find_element("xpath", "//a[contains(text(), 'Neogrid')]")$
      click()
    Sys.sleep(3) 
    
    neogrid <- list()
    num_paginas <- 3
    
    for (i in 1:num_paginas) {
      message(paste("Neogrid - Página", i))
      
      page_html <- session$get_page_source()
      html <- read_html(page_html)
      
      linhas <- html %>%
        html_element("#nfTableBody") %>%
        html_elements("tr")
      
      if (length(linhas) == 0) {
        message("Nenhuma linha encontrada na tabela Neogrid")
        break
      }
      
      dados <- map(linhas, function(tr) {
        tr %>% html_elements("td") %>% html_text(trim = TRUE)
      })
      
      dados <- dados[map_int(dados, length) > 0]
      
      if (length(dados) > 0) {
        neogrid_pag <- map_dfr(dados, function(x) {
          as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
        })
        
        neogrid[[i]] <- neogrid_pag
        
        next_btn <- try(
          session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
          silent = TRUE
        )
        
        if (!inherits(next_btn, "try-error")) {
          session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
          Sys.sleep(2)
          next_btn$click()
          Sys.sleep(3)
        } else {
          message("Botão 'Próxima' não encontrado, saindo do loop Neogrid")
          break
        }
      } else {
        message("Nenhum dado extraído da Neogrid")
        break
      }
    }
    
    if (length(neogrid) > 0) {
      tabela_neogrid <- bind_rows(neogrid)
      
      if(ncol(tabela_neogrid) >= 24) {
        colnames(tabela_neogrid) <- c("Item_Fatura", "Empresa", "Centro", "Data_Criacao", "Fornecedor",
                                    "Nome_Fornecedor", "Doc_Compras", "Item", "Material", "Descricao_Material",
                                    "Referencia", "Quantidade", "Preço_Contrato", "Preço_Fatura", "Montante",
                                    "Item_Contrato", "Data_Emissao", "Valor_NF_c/_Imposto", "Valor_NF_s/_Imposto",
                                    "ICMS", "PIS", "COFINS", "ICMS_Diferido", "IPI")[1:ncol(tabela_neogrid)]
        
        write_csv(tabela_neogrid, "tabela_neogrid.csv")
        message("Dados da Neogrid salvos com sucesso!")
      }
    }
    
    # Fecha o navegador
    session$close()
    message("Automação concluída com sucesso!")
    
  }, error = function(e) {
    message("Erro durante a execução: ", e$message)
    stop("Falha na automação")
  })
}

# Executar com retry
for (attempt in 1:max_retries) {
  message(paste("Tentativa", attempt, "de", max_retries))
  tryCatch({
    run_selenium_automation()
    message("Sucesso na tentativa ", attempt)
    break
  }, error = function(e) {
    message("Tentativa ", attempt, " falhou: ", e$message)
    if (attempt == max_retries) {
      stop("Todas as tentativas falharam: ", e$message)
    }
    message("Aguardando ", wait_time, " segundos para retry...")
    Sys.sleep(wait_time)
  })
}
