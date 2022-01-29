library(tidyverse)

## build code dictionary
theColNames <- readxl::read_xls("data/STC_Historical_2020/STC_Historical_DB (2020).xls",
                                #skip = 1,
                                #na = "X",
                                n_max = 1,
                                col_names = FALSE)
the_codes <- readxl::read_xls("data/STC_Historical_2020/STC_Historical_DB (2020).xls",
                              skip = 1,
                              #na = "X",
                              n_max = 1,
                              col_names = FALSE)

codes_dict <- as.list(the_codes)
names(codes_dict) <- theColNames[,5:36]

rm(the_codes)
rm(theColNames)

## read data

df <- readxl::read_xls("data/STC_Historical_2020/STC_Historical_DB (2020).xls",
                       #skip = 2,
                       na = "X")


df <- df[2:3815,]
df <- df %>% filter(Year>=2005)
df <- df %>% select(-c(2,4))
df <- df %>% mutate(State = str_sub(Name, 1,2))
df <- df %>% select(-Name)
df <- df %>% pivot_longer(C105:T99, names_to = "TaxClass", values_to = "TaxIncome")
df$TaxIncome <- as.numeric(df$TaxIncome)
df <- mutate(df, TaxName = as.character(codes_dict[TaxClass]))

mass_df <- df %>% filter(State == "MA")


ggplot(filter(mass_df, TaxClass == "C105"), aes(x = Year, y = TaxIncome)) +
  geom_point()

ggplot(filter(mass_df, TaxClass != "C105", Year == 2020, TaxIncome !=0, !is.na(TaxIncome) ), aes(x = TaxIncome, y = TaxName)) +
  geom_bar(stat = "identity")
