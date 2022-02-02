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
df_original <- readxl::read_xls("data/STC_Historical_2020/STC_Historical_DB (2020).xls",
                       #skip = 2,
                       na = "X")


df <- df_original[2:3815,] %>% 
  filter(Year>=2005) %>% 
  select(-c(State, `FY Ending Date`)) %>% 
  mutate(State = str_sub(Name, 1,2)) %>% 
  select(-Name) %>% 
  filter(State != "US")
  # select categories, not the subcategories

# fix class of columns
df[,2:33] <- lapply(df[,2:33], parse_number)
df$State <- as.factor(df$State)

# convert to millions of dollars, original is thousans of dollars
df[,2:33] <- df[,2:33]/1000

df_major_cat <- df %>% 
  select(Year, State, starts_with("C")) %>% 
  #remove the total column
  select(-C105)
# also make a long data frame
df_major_cat_long <- df_major_cat %>% 
  pivot_longer(C107:C130, names_to = "TaxCode", values_to = "Millions_dollars", values_drop_na = TRUE) %>% 
  #add readable description of tax code
  mutate(TaxName = as.character(codes_dict[TaxCode]))

# subcategories
df_sub_cat <- df %>% 
  select(Year, State, starts_with("T"))
# also make a long data frame
df_sub_cat_long <- df_sub_cat %>% 
  pivot_longer(T01:T99, names_to = "TaxCode", values_to = "Millions_dollars", values_drop_na = TRUE) %>% 
  #add readable description of tax code
  mutate(TaxName = as.character(codes_dict[TaxCode]))


## https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
pop_data <- read_csv("data/apportionment.csv")
pop_data <- pop_data %>% 
  filter(Year == 2010 | Year == 2020, `Geography Type` == "State") %>% 
  select(Name, Year, `Resident Population`) %>% 
  rename(Population = `Resident Population`,
         StateName = Name) %>% 
  filter(StateName %in% state.name)
pop_data$State <- as.factor(state.abb[match(pop_data$StateName, state.name)])

df <- df %>% pivot_longer(C105:T99, names_to = "TaxClass", values_to = "TaxIncome")
df$TaxIncome <- as.numeric(df$TaxIncome)
df_wide <- df %>% pivot_wider(names_from = TaxClass, values_from = TaxIncome)

df <- mutate(df, TaxName = as.character(codes_dict[TaxClass]))

mass_df <- df %>% filter(State == "MA")


ggplot(filter(mass_df, TaxClass == "C105"), aes(x = Year, y = TaxIncome)) +
  geom_point()

ggplot(filter(mass_df, TaxClass != "C105", Year == 2020, TaxIncome !=0, !is.na(TaxIncome) ), aes(x = TaxIncome, y = TaxName)) +
  geom_bar(stat = "identity")


cat_wide <- df_wide %>% select(Year, State, C107, C109, C118, C129, C130, C105 )
mutate(cat, total_tax = C107 + C109 + C118 + C129 + C130) %>% View()


ggplot(filter(df_major_cat, State == "MA"), aes(x = Year, y = C105)) +
  geom_line()

ggplot(filter(df_major_cat_long, State == "MA"), aes(x = Year, y = Millions_dollars, fill = TaxName)) +
  geom_area()


df_2015 <- df %>% filter(Year == 2015)

ggplot(filter(df_major_cat_long, Year == 2020), aes(y = Millions_dollars, color = TaxCode)) +
  geom_boxplot()


