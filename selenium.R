# install.packages("selenium-r")

library(selenium)
library(rvest)
library(dplyr)

#criar o server

#java -jar "C:\seleniumR\selenium-server-4.36.0.jar" standalone --port 4444
# no prompt de comando

#Abrir o chrome
# session <- SeleniumSession$new(browser = "chrome")
server <- selenium_server(temp = FALSE)

wait_for_server(server)

Sys.sleep(5)

session <- SeleniumSession$new()

Sys.sleep(5)

# Abrir o site
session$navigate("https://web-server-flask.onrender.com/")

Sys.sleep(10)

# Clica no botao login
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

########################################################################################################

# Clica na página SharePoint
session$
  find_element("xpath", "//a[contains(text(), 'SharePoint')]")$
  click()

Sys.sleep(5)

sharepoint <- list()

num_paginas <- 3

for (i in 1:num_paginas) {
  # Captura o HTML da página atual
  page_html <- session$get_page_source()
  html <- read_html(page_html)
  
  # Extrai as linhas da tabela
  linhas <- html %>%
    html_element("#table-body") %>%
    html_elements("tr")
  
  # Extrai as células (td)
  dados <- lapply(linhas, function(tr) {
    tr %>% html_elements("td") %>% html_text(trim = TRUE)
  })
  
  # Converte para data.frame
  sharepoint_pag <- do.call(rbind, lapply(dados, function(x) {
    as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
  }))
  
  sharepoint[[i]] <- sharepoint_pag
  
  # Tenta clicar em "Próxima", se existir
  next_btn <- try(
    session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
    silent = TRUE
  )
  
  if (!inherits(next_btn, "try-error")) {
    # Scroll até o botão usando o método do seleniumR
    session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(2) # espera o scroll
    
    # Clica no botão
    next_btn$click()
    Sys.sleep(3) 
  } else {
    break
  }
}

tabela_sharepoint <- bind_rows(sharepoint)

colnames(tabela_sharepoint) <- c("Codigo_SAP", "Codigo_Litigio", "Data_Abertura",
                            "Material", "Nota_Fiscal", "Data_Litigio",
                            "Status", "Fornecedor")

write.csv(tabela_sharepoint, "tabela_sharepoint.csv", row.names = FALSE, fileEncoding = "UTF-8")

#########################################################################################################
#SAP

#scrolla para cima
session$execute_script("window.scrollTo(0, -document.body.scrollHeight);")

Sys.sleep(2)

# Clica na página Sap
session$
  find_element("xpath", "//a[contains(text(), 'Sap')]")$
  click()

Sys.sleep(3) 

sap <- list()

num_paginas <- 3

for (i in 1:num_paginas) {
  
  # Captura o HTML da página atual
  page_html <- session$get_page_source()
  html <- read_html(page_html)
  
  # Extrai as linhas da tabela (tbody com id='litigios-body')
  linhas <- html %>%
    html_element("#litigios-body") %>%
    html_elements("tr")
  
  # Extrai as células de cada linha
  dados <- lapply(linhas, function(tr) {
    tr %>% html_elements("td") %>% html_text(trim = TRUE)
  })
  
  # Converte cada linha em data.frame
  sap_pag <- do.call(rbind, lapply(dados, function(x) {
    as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
  }))
  
  sap[[i]] <- sap_pag
  
  # Tenta clicar em "Próxima", se existir
  next_btn <- try(
    session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
    silent = TRUE
  )
  
  if (!inherits(next_btn, "try-error")) {
    # Scroll até o botão usando o método do seleniumR
    session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(2) # espera o scroll
    
    # Clica no botão
    next_btn$click()
    Sys.sleep(3) # espera a página carregar
  } else {
    break
  }
}

tabela_sap <- bind_rows(sap)

colnames(tabela_sap) <- c("Codigo_Litigio", "Item_Fatura", "Status", "Empresa", "Centro",
                            "Grupo_Compradores", "Data_Criacao", "Fornecedor", "Nome_Fornecedor",
                            "Documento_Compras", "Item", "Material", "Descricao_Material",
                            "Referencia", "Quantidade", "Preco_Contrato", "Preco_Fatura",
                            "Valor_Diferenca", "Montante", "Item_Contrato")

# Salva em CSV
write.csv(tabela_sap, "tabela_sap.csv", row.names = FALSE, fileEncoding = "UTF-8")

#########################################################################################################
#NEOGRID

# Scrolla para cima
session$execute_script("window.scrollTo(0, -document.body.scrollHeight);")

Sys.sleep(2)

# Clica em página Neogrid
session$
  find_element("xpath", "//a[contains(text(), 'Neogrid')]")$
  click()

Sys.sleep(3) 

neogrid <- list()

num_paginas <- 3

for (i in 1:num_paginas) {
  
  # Captura o HTML da página atual
  page_html <- session$get_page_source()
  html <- read_html(page_html)
  
  # Extrai as linhas da tabela
  linhas <- html %>%
    html_element("#nfTableBody") %>%
    html_elements("tr")
  
  # Extrai as células (td)
  dados <- lapply(linhas, function(tr) {
    tr %>% html_elements("td") %>% html_text(trim = TRUE)
  })
  
  # Converte para data.frame
  neogrid_pag <- do.call(rbind, lapply(dados, function(x) {
    as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE)
  }))
  
  neogrid[[i]] <- neogrid_pag
  
  # Tenta clicar em "Próxima", se existir
  next_btn <- try(
    session$find_element("xpath", "//button[.//span[contains(text(), 'Próxima')]]"),
    silent = TRUE
  )
  
  if (!inherits(next_btn, "try-error")) {
    # Scroll até o botão usando o método do seleniumR
    session$execute_script("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(2) # espera o scroll
    
    # Clica no botão
    next_btn$click()
    Sys.sleep(3) # espera a página carregar
  } else {
    break
  }
}

tabela_neogrid <- bind_rows(neogrid)

colnames(tabela_neogrid) <- c("Item_Fatura", "Empresa", "Centro", "Data_Criacao", "Fornecedor",
                            "Nome_Fornecedor", "Doc_Compras", "Item", "Material", "Descricao_Material",
                            "Referencia", "Quantidade", "Preço_Contrato", "Preço_Fatura", "Montante",
                            "Item_Contrato", "Data_Emissao", "Valor_NF_c/_Imposto", "Valor_NF_s/_Imposto",
                            "ICMS", "PIS", "COFINS", "ICMS_Diferido", "IPI")

write.csv(tabela_neogrid, "tabela_neogrid.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Fecha o navegador
session$close()


