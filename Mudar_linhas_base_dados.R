


# Descrição da base de dados

#|Nome Variável           |Novo nome Variável|Tipo Variável     |Descrição variável
#|<-----------------------|<-----------------|<-----------------|<----------------------------------------|
#|Tipo Demanda            |Motivação_Compra  |Categórica nominal|Necessidade do cliente que gerou a compra|
#|Oferta  Oportunidade    |Tipo_Produto      |Categórica nominal|Categoria do produto vendido             |
#|Parceiro                |Parceiro          |Categórica nominal|Empresa parceira associada               |
#|Vendedor                |Nome_Vendedor     |Categórica nominal|Vendedor da empresa associada            |
#|Origem                  |Origem_Venda      |Categórica nominal|Por onde foi prospectada a venda         |
#|UF                      |UF                |Categórica nominal|Estado que realizou a venda              |
#|Ramo  de  Atividade     |Ramo_Atividade    |Categórica nominal|Ramo de atividade que adquirou a solução |
#|Vl  Fech  Total  (Meta )|Valor_Venda       |Numérica          |Valor da venda em reais                  |



# 2.0 Base de dados

# 2.0.1 Importar base original

base_original_tactium <- read_excel("Base_dados.xlsx")
base_original_tactium %>% kable() %>% kable_styling(bootstrap_options = "striped",
                                                    full_width = FALSE,
                                                    font_size = 13)

# 2.0.2 Filtrar dados que importam da base original
base_selecionada_tactium <- base_original_tactium %>% select(`Tipo  Demanda`, `Oferta  Oportunidade`, Parceiro,
                                                            Vendedor, Origem, UF, `Ramo  de  Atividade`, `Vl  Fech  Total  (Meta )`)

base_selecionada_tactium %>% kable() %>% kable_styling(bootstrap_options = "striped",
                                                      full_width = FALSE,
                                                      font_size = 13)
# 2.0.3 Mudar nome das colunas

base_selecionada_tactium <- base_selecionada_tactium %>% rename("Valor_Venda" = "Vl  Fech  Total  (Meta )",
                                                              "Motivação_Compra" = "Tipo  Demanda","Tipo_Produto" = "Oferta  Oportunidade",
                                                              "Nome_Vendedor" = "Vendedor","Origem_Venda" = "Origem", 
                                                              "Ramo_Atividade" = "Ramo  de  Atividade")
base_selecionada_tactium %>% kable() %>% kable_styling(bootstrap_options = "striped",
                                                       full_width = FALSE,
                                                       font_size = 13)

skimr::skim(base_selecionada_tactium)

# 3.0 Mudar valores da base

base_selecionada_tactium[-8] %>% purrr::map(unique)

# 3.0.1 Mudar valores da motivação da compra

Motivo3 = c("Venda/Ampliação" = "Recrutar_Tropas",
"Outros Serviços" = "Manunteção_Robos",
"Customização" = "Comprar_Caças",
"Treinamento" = "Comprar_Armamento",
"Visita/Suporte (Impl)" = "Combustível",
"Suporte/Atualização sem Contrato" = "Ração",
"Venda Fabricante - Cliente" = "Fardamento",
"Venda Fabricante -> Cliente" = "Treinamento"
)


base_selecionada_tactium$Motivação_Compra <- str_replace_all(base_selecionada_tactium$Motivação_Compra,Motivo3)
base_selecionada_tactium$Motivação_Compra <- str_replace(base_selecionada_tactium$Motivação_Compra,"^Visita/Suporte\\s\\(Impl\\)","Combustível")



# 3.0.2 Mudar valores do Tipo do produto
unique(base_selecionada_tactium$Tipo_Produto)
tipo = c("Produto'" = "Recrutador_M",
         "Produto2" = "Robot_Hlp",
         "Produto3" = "X_Wing",
         "Produto4" = "Blaster_Master",
         "Produto5" = "Gass",
         "Produto6" = "Pills_Food",
         "Produto7" = "Elmor",
         "Produto8" = "BJJ_Kombat",
         "Produto9" = "Sex_Pistols",
         "Produto10" = "Jet_Fast",
         "Produto11" = "Bot_Ensable",
         "Produto12" = "Air_Supply"
)

base_selecionada_tactium$Tipo_Produto <- str_replace_all(base_selecionada_tactium$Tipo_Produto, tipo)
base_selecionada_tactium$Tipo_Produto <- str_replace(base_selecionada_tactium$Tipo_Produto,"^X_Wing\\+IP","Robot_Hlp")


# 3.0.3 Mudar valores do Parceiro
c(unique(base_selecionada_tactium$Parceiro))
parceiro = c("Parceiro1" = "Luke_Coorp",
             "Parceiro2" = "D_Stars",
             "Parceiro3" = "OCP",
             "Parceiro4" = "R2D2",
             "Parceiro5" = "Sith_Inc",
             "Parceiro6" = "Abdala&Cia",
             "Parceiro7" = "B1Q9_Co",
             "Parceiro8" = "ArpaNet",
             "Parceiro9" = "SkyLine",
             "Parceiro10" = "Bk&Rogers",
             "Parceiro11" = "FwthYou",
             "Parceiro12" = "YDA",
             "Parceiro13" = "F&Millennium",
             "Parceiro14" = "GDie_Ind",
             "Parceiro15" = "Endor"
)


base_selecionada_tactium$Parceiro <-str_replace_all(base_selecionada_tactium$Parceiro,parceiro)

# 3.0.4 Mudar valores do Nome do vendedor
unique(base_selecionada_tactium$Nome_Vendedor)
vendedor = c("Vendedor1" = "Lando",
         "Vendedor2" = "Padmé",
         "Vendedor3 - ME" = "Han",
         "Vendedor4" = "Chew",
         "Vendedor5 Carvalho" = "Vader",
         "Vendedor6" = "Finn",
         "Vendedor7" = "Ben",
         "Vendedor8" = "Jar Jar",
         "Vendedor9" = "Poe",
         "Vendedor10" = "Jabba",
         "Vendedor11" = "Holdo"
)

base_selecionada_tactium$Nome_Vendedor <- str_replace_all(base_selecionada_tactium$Nome_Vendedor,vendedor)


# 3.0.5 Mudar valores do Nome do vendedor
unique(base_selecionada_tactium$UF)
uf = c("Planeta1" = "Mirial",
             "Planeta2" = "Shili",
             "Planeta3" = "Dagobah",
             "Planeta4" = "Akiva",
             "Planeta5" = "Tatooine",
             "Planeta6" = "Geonosis",
             "Planeta7" = "Mygeeto",
             "Planeta8" = "Rodia",
             "Planeta9" = "Plexis",
             "Planeta10" = "Yavin",
             "Planeta11" = "Jedha",
             "Planeta12" = "Kiris 6",
             "Planeta13" = "Naboo",
             "Planeta14" = "Sembla",
             "Planeta15" = "Scarif",
             "Planeta16" = "Mirial",
             "Planeta17" = "Ryloth",
             "Planeta18" = "Krant"
)


base_selecionada_tactium$UF <- str_replace_all(base_selecionada_tactium$UF,uf)

# 3.0.6 Mudar valores do Ramo de Atividade
unique(base_selecionada_tactium$Ramo_Atividade)
length(unique(base_selecionada_tactium$Ramo_Atividade))
ramo = c("Ramo1" = "Centro Comunicação",
       "Ramo2" = "Engenharia",
       "Ramo3" = "Telemetria",
       "Ramo4" = "Siderurgia",
       "Ramo5" = "Contrainformação",
       "Ramo6" = "Artilharia",
       "Ramo7" = "Centro Informação",
       "Ramo8" = "Transporte",
       "Ramo9" = "Feiras",
       "Ramo10" = "Governo",
       "Ramo11" = "Cyber Segurança",
       "Ramo12" = "Pequena Indústria",
       "Ramo13" = "Induústria Alimentos",
       "Ramo14" = "Centro Informação",
       "Ramo15" = "Infantaria",
       "Ramo16" = "Demolição",
       "(Vazio)" = "Telemetria"
)

base_selecionada_tactium$Ramo_Atividade <- str_replace_all(base_selecionada_tactium$Ramo_Atividade,ramo)
base_selecionada_tactium$Ramo_Atividade <- str_replace(base_selecionada_tactium$Ramo_Atividade,"\\(Telemetria\\)","Telemetria")

# 3.0.7 Mudar valores das vendas

base_selecionada_tactium$Valor_Venda <- base_selecionada_tactium$Valor_Venda*3.14159265359



# 3.0.8 Modificar nome coluna UF

base_selecionada_tactium <- base_selecionada_tactium %>% rename(Vendedor = Nome_Vendedor, Produto = Tipo_Produto, Motivo_Compra = Motivação_Compra)
base_selecionada_tactium %>% kable() %>% kable_styling(bootstrap_options = "striped",
                                                       full_width = FALSE,
                                                       font_size = 13)
# 4.0 Gravar base de dados

write_xlsx(base_selecionada_tactium,"C:\\Desenvolvimento\\DataScience\\ASN\\10 - Ávore de decisão\\Comercio_galatico.xlsx")








