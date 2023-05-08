Address Term 2023
================
Soobin Choi
2023-05-01

- <a href="#data-processing" id="toc-data-processing">Data Processing</a>
- <a href="#address-term" id="toc-address-term">Address Term</a>
  - <a href="#analysis--plots" id="toc-analysis--plots">Analysis &amp;
    Plots</a>
    - <a href="#the-usage-of-dude-by-gender-and-age"
      id="toc-the-usage-of-dude-by-gender-and-age">The usage of <em>dude</em>
      by gender and age</a>
    - <a href="#compare-dude-and-bro-by-resp--addr-gender"
      id="toc-compare-dude-and-bro-by-resp--addr-gender">Compare <em>dude</em>
      and <em>bro</em> by RESP &amp; ADDR gender</a>
    - <a href="#is-there-any-difference-in-the-usage-of-dude-by-race"
      id="toc-is-there-any-difference-in-the-usage-of-dude-by-race">Is there
      any difference in the usage of <em>dude</em> by race?</a>
- <a href="#sentence-rank" id="toc-sentence-rank">Sentence Rank</a>
  - <a href="#data-processing-1" id="toc-data-processing-1">Data
    Processing</a>
  - <a href="#analysis" id="toc-analysis">Analysis</a>
    - <a href="#organize-by-question" id="toc-organize-by-question">Organize
      by question</a>
- <a href="#bro-vs-bruh---difference"
  id="toc-bro-vs-bruh---difference"><em>Bro</em> vs. <em>Bruh</em> -
  Difference</a>
  - <a href="#data-processing-2" id="toc-data-processing-2">Data
    Processing</a>
  - <a href="#analysis-1" id="toc-analysis-1">Analysis?</a>
    - <a href="#plot" id="toc-plot">Plot</a>

``` r
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment="")
library(tidyverse)
library(dplyr)
library(ggplot2)
```

# Data Processing

``` r
original <- read_csv("data/original_num.csv")
```

    Rows: 458 Columns: 79
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (8): ResponseId, AT ever use, GENDER, GENDER_4_TEXT, SEXUALITY, AGE, RA...
    dbl (71): LANGUAGE, BRO-F-parent, BRO-F-sibling, BRO-F-partner, BRO-F-cowork...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(original)
```

    # A tibble: 6 × 79
      ResponseId  AT ev…¹ GENDER GENDE…² SEXUA…³ AGE   LANGU…⁴ RACE  OCCUP…⁵ BRO-F…⁶
      <chr>       <chr>   <chr>  <chr>   <chr>   <chr>   <dbl> <chr> <chr>     <dbl>
    1 R_9FRUk95l… dude,m… Non-b… <NA>    Gay     18-25       1 White Editor…      NA
    2 R_25EnKuXy… dude,g… Femin… <NA>    Hetero  26-40       1 white Studen…      NA
    3 R_2pLWRKVY… bro,du… Femin… <NA>    Bisexu… 26-40       1 White PhD st…       1
    4 R_PBtJhKNv… bro,du… Mascu… <NA>    Straig… 26-40       1 White Teacher       1
    5 R_22wZhXw6… dude,m… Mascu… <NA>    Straig… 41-55       1 White Civil …      NA
    6 R_305B45bv… bro,du… Femin… <NA>    Bisexu… 18-25       1 White Econom…       1
    # … with 69 more variables: `BRO-F-sibling` <dbl>, `BRO-F-partner` <dbl>,
    #   `BRO-F-coworker` <dbl>, `BRO-F-boss` <dbl>, `BRO-F-friend` <dbl>,
    #   `BRO-F-stranger` <dbl>, `BRO-M-parent` <dbl>, `BRO-M-sibling` <dbl>,
    #   `BRO-M-partner` <dbl>, `BRO-M-coworker` <dbl>, `BRO-M-boss` <dbl>,
    #   `BRO-M-friend` <dbl>, `BRO-M-stranger` <dbl>, `BRUH-F-parent` <dbl>,
    #   `BRUH-F-sibling` <dbl>, `BRUH-F-partner` <dbl>, `BRUH-F-coworker` <dbl>,
    #   `BRUH-F-boss` <dbl>, `BRUH-F-friend` <dbl>, `BRUH-F-stranger` <dbl>, …

``` r
# process the column for the analysis - term, addressee gender, relationship
# And, change the name of columns

original1 <- original %>% 
  pivot_longer(`BRO-F-parent`:`MAN-F-stranger`, names_to = "TERM", values_to = "FREQUENCY") %>% 
  separate(col = TERM, c("TERM", "ADDR-GENDER", "RANK"), sep = "-") %>% 
  rename(`RESP-ID` = ResponseId,
         ATUSE = `AT ever use`,
         `RESP-GENDER` = GENDER) %>% 
  mutate(RANK = str_to_upper(RANK))
```

``` r
# check the categories in AGE column
original1 %>% 
  select(AGE) %>% 
  unique()
```

    # A tibble: 6 × 1
      AGE    
      <chr>  
    1 18-25  
    2 26-40  
    3 41-55  
    4 56-70  
    5 Over 70
    6 <NA>   

``` r
original2 <- original1 %>% 
# remove NAs in AGE column
  filter(!is.na(AGE)) %>% 
  mutate(`RESP-GENDER` = str_to_title(`RESP-GENDER`)) %>%
  relocate(`ADDR-GENDER`, .after="RESP-GENDER") %>% 
  # merge '56-70' and 'over 70' into one category
  mutate_all(funs(str_replace_all(., "56-70", "Over 70"))) %>% 
  mutate_all(funs(str_replace_all(., "Over 70", "56-Over 70"))) %>% 
  # change 'other' gender to 'non-binary'
  map_dfr(~ str_replace_all(., "Other", "Non-Binary"))
```

    Warning: `funs()` was deprecated in dplyr 0.8.0.
    Please use a list of either functions or lambdas: 

      # Simple named list: 
      list(mean = mean, median = median)

      # Auto named with `tibble::lst()`: 
      tibble::lst(mean, median)

      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
# make another column - clean up `SEXUALITY` column as binary value
org_clean <- original2 %>%
  mutate(SEXUALITY2 = grepl("(het|straight)", tolower(original2$SEXUALITY))) %>% 
  relocate(SEXUALITY2, .after = SEXUALITY) %>% 
  map_dfr(~ str_replace_all(., c("TRUE" = "Hetero", "FALSE" = "Non-Hetero"))) %>% 
  mutate(FREQUENCY = as.numeric(FREQUENCY))
```

Okay, I have left with 31,990 values in total.

# Address Term

## Analysis & Plots

### The usage of *dude* by gender and age

``` r
# make new column called 'DUDE': TRUE means the participant uses *dude*, FALSE means they don't

dude_df <- org_clean %>% 
  mutate(DUDE = grepl("dude", org_clean$ATUSE))
head(dude_df, 10)
```

    # A tibble: 10 × 15
       `RESP-ID`   ATUSE RESP-…¹ ADDR-…² GENDE…³ SEXUA…⁴ SEXUA…⁵ AGE   LANGU…⁶ RACE 
       <chr>       <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>   <chr>
     1 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     2 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     3 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     4 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     5 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     6 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     7 R_9FRUk95l… dude… Non-Bi… F       <NA>    Gay     Non-He… 18-25 1       White
     8 R_9FRUk95l… dude… Non-Bi… M       <NA>    Gay     Non-He… 18-25 1       White
     9 R_9FRUk95l… dude… Non-Bi… M       <NA>    Gay     Non-He… 18-25 1       White
    10 R_9FRUk95l… dude… Non-Bi… M       <NA>    Gay     Non-He… 18-25 1       White
    # … with 5 more variables: OCCUPATION <chr>, TERM <chr>, RANK <chr>,
    #   FREQUENCY <dbl>, DUDE <lgl>, and abbreviated variable names ¹​`RESP-GENDER`,
    #   ²​`ADDR-GENDER`, ³​GENDER_4_TEXT, ⁴​SEXUALITY, ⁵​SEXUALITY2, ⁶​LANGUAGE

#### *Dude* by RESP-gender and age

``` r
# Plot - Dude Usage by gender and age
dude_df %>% 
  group_by(`RESP-GENDER`, AGE, DUDE) %>% 
  summarise(Count = n_distinct(`RESP-ID`)) %>% 
  ggplot(aes(x = AGE, y = Count, fill = DUDE)) + 
  facet_wrap(vars(`RESP-GENDER`), ncol = 3) + 
  geom_col(position = "fill") +
  labs(title = "Dude Usage by Respondants' Gender and Age")
```

    `summarise()` has grouped output by 'RESP-GENDER', 'AGE'. You can override
    using the `.groups` argument.

![](AT2023_code_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
'
dude_df %>% 
  filter(TERM == "DUDE" & `RESP-GENDER` != "Non-Binary") %>% 
  group_by(AGE, `ADDR-GENDER`, `RESP-GENDER`) %>% 
  summarise(count = n_distinct(`RESP-ID`)) %>% 
  unite(Gender, `RESP-GENDER`, `ADDR-GENDER`) %>% 
  ggplot(aes(x = AGE, weight = count, fill = Gender)) +
  geom_bar(position = "dodge")
'
```

    [1] "\ndude_df %>% \n  filter(TERM == \"DUDE\" & `RESP-GENDER` != \"Non-Binary\") %>% \n  group_by(AGE, `ADDR-GENDER`, `RESP-GENDER`) %>% \n  summarise(count = n_distinct(`RESP-ID`)) %>% \n  unite(Gender, `RESP-GENDER`, `ADDR-GENDER`) %>% \n  ggplot(aes(x = AGE, weight = count, fill = Gender)) +\n  geom_bar(position = \"dodge\")\n"

#### *Dude* respondent’s gender only

``` r
# consider respondent's gender only - total count of people who answered that they use dude

dude_df %>% 
  group_by(`RESP-GENDER`, DUDE) %>% 
  summarise(Count = n_distinct(`RESP-ID`))
```

    `summarise()` has grouped output by 'RESP-GENDER'. You can override using the
    `.groups` argument.

    # A tibble: 6 × 3
    # Groups:   RESP-GENDER [3]
      `RESP-GENDER` DUDE  Count
      <chr>         <lgl> <int>
    1 Feminine      FALSE    74
    2 Feminine      TRUE    211
    3 Masculine     FALSE    28
    4 Masculine     TRUE     90
    5 Non-Binary    FALSE    10
    6 Non-Binary    TRUE     44

``` r
dude_df %>% 
  group_by(`RESP-GENDER`, DUDE) %>% 
  summarise(Count = n_distinct(`RESP-ID`)) %>% 
  ggplot(aes(x = `RESP-GENDER`, y = Count, fill = DUDE)) + 
  geom_col(position = "fill") + 
  ylab("Percentage") + 
  labs(title = "Dude Usage by Respondents' Gender")
```

    `summarise()` has grouped output by 'RESP-GENDER'. You can override using the
    `.groups` argument.

![](AT2023_code_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### *Dude* - Considering RESP & ADDR gender

``` r
dude_df %>% 
  filter(TERM == "DUDE") %>% 
  filter(!is.na(FREQUENCY)) %>% 
  group_by(`ADDR-GENDER`, `RESP-GENDER`) %>% 
  summarise(mean_freq = mean(FREQUENCY)) %>% 
  ggplot(aes(x = `RESP-GENDER`, y = mean_freq, col = `ADDR-GENDER`, group = `ADDR-GENDER`))+
  geom_point() + 
  geom_text(aes(label = round(mean_freq, 2), vjust = -0.5, nudge_y = 0.1)) +
  geom_path() +
  labs(title = "Reported Frequency of Dude by Gender of Speaker and Addressee") +
  ylim(1.9, 2.65)
```

    `summarise()` has grouped output by 'ADDR-GENDER'. You can override using the
    `.groups` argument.

    Warning: Ignoring unknown aesthetics: nudge_y

![](AT2023_code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Compare *dude* and *bro* by RESP & ADDR gender

``` r
# sort people who use dude and/or bro

dudebro <- org_clean %>% 
  filter(grepl("dude", org_clean$ATUSE) == TRUE | grepl("bro", org_clean$ATUSE) == TRUE)
```

``` r
# plot - dude/bro usage by rank and gender

dudebro %>% 
  filter(TERM %in% c("DUDE", "BRO")) %>% 
  group_by(`RESP-GENDER`, `ADDR-GENDER`, TERM, `RANK`) %>% 
  summarise(mean_freq = mean(as.numeric(FREQUENCY), na.rm = TRUE)) %>% 
  ggplot(aes(x = `RANK`, y = mean_freq,  group = TERM, col = TERM)) +
  facet_wrap(`ADDR-GENDER`~`RESP-GENDER`, ncol = 1, strip.position = "right", 
             labeller = labeller(
               `ADDR-GENDER` = c(`F`="Addr_F", `M`="Addr_M"),
               `RESP-GENDER` = c(`Feminine`="Resp_F", `Masculine`="Resp_M", `Non-Binary`="Non-Binary"))) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(limits = c(0, 4)) +
  labs(title = "Compare dude/bro based on interlocutors' gender", x = 'Relationship')
```

    `summarise()` has grouped output by 'RESP-GENDER', 'ADDR-GENDER', 'TERM'. You
    can override using the `.groups` argument.

![](AT2023_code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

it contains too much information in one plot. need to make it more
reader-friendly

``` r
# DUDE
dudebro %>% 
  filter(TERM == "DUDE") %>% 
  group_by(`RESP-GENDER`, `ADDR-GENDER`, TERM, `RANK`) %>% 
  summarise(mean_freq = mean(as.numeric(FREQUENCY), na.rm = TRUE)) %>% 
  unite(GENDER, `RESP-GENDER`, `ADDR-GENDER`)
```

    `summarise()` has grouped output by 'RESP-GENDER', 'ADDR-GENDER', 'TERM'. You
    can override using the `.groups` argument.

    # A tibble: 42 × 4
    # Groups:   TERM [1]
       GENDER     TERM  RANK     mean_freq
       <chr>      <chr> <chr>        <dbl>
     1 Feminine_F DUDE  BOSS          1.32
     2 Feminine_F DUDE  COWORKER      2.31
     3 Feminine_F DUDE  FRIEND        3.04
     4 Feminine_F DUDE  PARENT        1.40
     5 Feminine_F DUDE  PARTNER       2.10
     6 Feminine_F DUDE  SIBLING       2.22
     7 Feminine_F DUDE  STRANGER      1.70
     8 Feminine_M DUDE  BOSS          1.34
     9 Feminine_M DUDE  COWORKER      2.44
    10 Feminine_M DUDE  FRIEND        3.19
    # … with 32 more rows

``` r
dudebro %>% 
  filter(TERM == "DUDE") %>% 
  group_by(`RESP-GENDER`, `ADDR-GENDER`, TERM, `RANK`) %>% 
  summarise(mean_freq = mean(as.numeric(FREQUENCY), na.rm = TRUE)) %>% 
  unite(GENDER, `RESP-GENDER`, `ADDR-GENDER`) %>% 
  ggplot(aes(x = RANK, y = mean_freq, col = GENDER, group = GENDER)) +
  geom_point() +
  geom_line() + 
  xlab("Relationship") +
  labs(title = "Dude Usage by Gender") + 
  ylim(0.5, 4)
```

    `summarise()` has grouped output by 'RESP-GENDER', 'ADDR-GENDER', 'TERM'. You
    can override using the `.groups` argument.

![](AT2023_code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# BRO

dudebro %>% 
  filter(TERM == "BRO") %>% 
  group_by(`RESP-GENDER`, `ADDR-GENDER`, TERM, `RANK`) %>% 
  summarise(mean_freq = mean(as.numeric(FREQUENCY), na.rm = TRUE)) %>% 
  unite(GENDER, `RESP-GENDER`, `ADDR-GENDER`)
```

    `summarise()` has grouped output by 'RESP-GENDER', 'ADDR-GENDER', 'TERM'. You
    can override using the `.groups` argument.

    # A tibble: 42 × 4
    # Groups:   TERM [1]
       GENDER     TERM  RANK     mean_freq
       <chr>      <chr> <chr>        <dbl>
     1 Feminine_F BRO   BOSS          1.21
     2 Feminine_F BRO   COWORKER      1.86
     3 Feminine_F BRO   FRIEND        2.89
     4 Feminine_F BRO   PARENT        1.21
     5 Feminine_F BRO   PARTNER       1.71
     6 Feminine_F BRO   SIBLING       2.20
     7 Feminine_F BRO   STRANGER      1.57
     8 Feminine_M BRO   BOSS          1.19
     9 Feminine_M BRO   COWORKER      2.05
    10 Feminine_M BRO   FRIEND        3.19
    # … with 32 more rows

``` r
dudebro %>% 
  filter(TERM == "BRO") %>% 
  group_by(`RESP-GENDER`, `ADDR-GENDER`, TERM, `RANK`) %>% 
  summarise(mean_freq = mean(as.numeric(FREQUENCY), na.rm = TRUE)) %>% 
  unite(GENDER, `RESP-GENDER`, `ADDR-GENDER`) %>% 
  ggplot(aes(x = RANK, y = mean_freq, col = GENDER, group = GENDER)) +
  geom_point() +
  geom_line() +
  xlab("Relationship") +
  labs(title = "Bro Usage by Gender") + 
  ylim(0.5, 4)
```

    `summarise()` has grouped output by 'RESP-GENDER', 'ADDR-GENDER', 'TERM'. You
    can override using the `.groups` argument.

![](AT2023_code_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

### Is there any difference in the usage of *dude* by race?

``` r
org_clean %>% 
  mutate(RACE = str_to_title(RACE)) %>% 
  group_by(RACE) %>% 
  summarize(n = n_distinct(`RESP-ID`))
```

    # A tibble: 82 × 2
       RACE                         n
       <chr>                    <int>
     1 African American             1
     2 Anglo                        1
     3 Anglo Australian (White)     1
     4 Arab                         1
     5 Ashkenazi Jewish             1
     6 Asian                        7
     7 Asian-American               1
     8 Asian Indian                 2
     9 Asian, Korean                1
    10 Australian                   1
    # … with 72 more rows

# Sentence Rank

## Data Processing

``` r
# load the data
rank_sent <- read_csv("data/rank_sent.csv")
```

    Rows: 458 Columns: 31
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (7): ResponseId, AT ever use, RESP-GENDER, GENDER-4-TEXT, SEXUALITY, AG...
    dbl (24): MUSIC-BRO, MUSIC-GIRL, MUSIC-DUDE, MUSIC-BRUH, MEETING-BRO, MEETIN...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(rank_sent)
```

    # A tibble: 6 × 31
      Respon…¹ AT ev…² RESP-…³ GENDE…⁴ SEXUA…⁵ AGE   MUSIC…⁶ MUSIC…⁷ MUSIC…⁸ MUSIC…⁹
      <chr>    <chr>   <chr>   <chr>   <chr>   <chr>   <dbl>   <dbl>   <dbl>   <dbl>
    1 R_9FRUk… dude,m… Non-bi… <NA>    Gay     18-25       2       4       1       3
    2 R_25EnK… dude,g… Femini… <NA>    Hetero  26-40       3       2       1       4
    3 R_2pLWR… bro,du… Femini… <NA>    Bisexu… 26-40       3       2       1       4
    4 R_PBtJh… bro,du… Mascul… <NA>    Straig… 26-40       2       3       1       4
    5 R_22wZh… dude,m… Mascul… <NA>    Straig… 41-55       2       3       1       4
    6 R_305B4… bro,du… Femini… <NA>    Bisexu… 18-25       3       2       1       4
    # … with 21 more variables: `MEETING-BRO` <dbl>, `MEETING-GIRL` <dbl>,
    #   `MEETING-DUDE` <dbl>, `MEETING-BRUH` <dbl>, `SHUTUP-BRO` <dbl>,
    #   `SHUTUP-GIRL` <dbl>, `SHUTUP-DUDE` <dbl>, `SHUTUP-BRUH` <dbl>,
    #   `GOODTOSEE-BRO` <dbl>, `GOODTOSEE-GIRL` <dbl>, `GOODTOSEE-DUDE` <dbl>,
    #   `GOODTOSEE-BRUH` <dbl>, `SIGNAL-BRO` <dbl>, `SIGNAL-GIRL` <dbl>,
    #   `SIGNAL-DUDE` <dbl>, `SIGNAL-BRUH` <dbl>, `EMAIL-BRO` <dbl>,
    #   `EMAIL-GIRL` <dbl>, `EMAIL-DUDE` <dbl>, `EMAIL-BRUH` <dbl>, Q23 <chr>, …

``` r
rank_clean <- rank_sent %>% 
  select(-Q23) %>% 
  rename(`RESP-ID` = ResponseId,
         ATUSE = `AT ever use`)
```

## Analysis

### Organize by question

#### MEETING

``` r
colnames(rank_clean)
```

     [1] "RESP-ID"        "ATUSE"          "RESP-GENDER"    "GENDER-4-TEXT" 
     [5] "SEXUALITY"      "AGE"            "MUSIC-BRO"      "MUSIC-GIRL"    
     [9] "MUSIC-DUDE"     "MUSIC-BRUH"     "MEETING-BRO"    "MEETING-GIRL"  
    [13] "MEETING-DUDE"   "MEETING-BRUH"   "SHUTUP-BRO"     "SHUTUP-GIRL"   
    [17] "SHUTUP-DUDE"    "SHUTUP-BRUH"    "GOODTOSEE-BRO"  "GOODTOSEE-GIRL"
    [21] "GOODTOSEE-DUDE" "GOODTOSEE-BRUH" "SIGNAL-BRO"     "SIGNAL-GIRL"   
    [25] "SIGNAL-DUDE"    "SIGNAL-BRUH"    "EMAIL-BRO"      "EMAIL-GIRL"    
    [29] "EMAIL-DUDE"     "EMAIL-BRUH"    

``` r
rank_clean %>% 
  select(contains(c("ID", "MUSIC")))
```

    # A tibble: 458 × 5
       `RESP-ID`         `MUSIC-BRO` `MUSIC-GIRL` `MUSIC-DUDE` `MUSIC-BRUH`
       <chr>                   <dbl>        <dbl>        <dbl>        <dbl>
     1 R_9FRUk95l1fWKDZf           2            4            1            3
     2 R_25EnKuXyTReLPE2           3            2            1            4
     3 R_2pLWRKVYVALdO6e           3            2            1            4
     4 R_PBtJhKNv0JB10GJ           2            3            1            4
     5 R_22wZhXw6DTa6krK           2            3            1            4
     6 R_305B45bvuFeKZUx           3            2            1            4
     7 R_vjZzVyscBlf29AB           3            2            1            4
     8 R_1IL6Ojmmkco93Hi           2            4            1            3
     9 R_0W1f5Y9lpxf2LWV           2            3            1            4
    10 R_3CCJoj9CLivBnt1           2            4            1            3
    # … with 448 more rows

``` r
meeting <- rank_clean %>% 
  select(contains(c("ID", "MEETING")))

meeting_final <- meeting %>% 
  rename_with(., ~gsub("MEETING-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Meeting", .after = TERM)

meeting %>% 
  rename_with(., ~gsub("MEETING-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  ggplot(aes(x = TERM, y = mean_rank)) +
  geom_path(group = 1, color = "dark green") +
  geom_point(color = "dark green") + 
  geom_text(aes(label = round(mean_rank, 2), hjust = -0.7, nudge_x = 0.1), color = "dark green") +
  scale_y_reverse(limits = c(5, 0.5)) +
  labs(title = "Mean Rank - \"When is the meeting?\"")
```

    Warning: Ignoring unknown aesthetics: nudge_x

![](AT2023_code_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

#### MUSIC

``` r
music <- rank_clean %>% 
  select(contains(c("ID", "MUSIC")))

music_final <- music %>% 
  rename_with(., ~gsub("MUSIC-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Music", .after = TERM)
```

#### SHUTUP

``` r
shutup <- rank_clean %>% 
  select(contains(c("ID", "SHUTUP")))

shutup_final <- shutup %>% 
  rename_with(., ~gsub("SHUTUP-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Shut up", .after = TERM)
```

#### GOODTOSEE

``` r
goodtosee <- rank_clean %>% 
  select(contains(c("ID", "GOODTOSEE")))

goodtosee_final <- goodtosee %>% 
  rename_with(., ~gsub("GOODTOSEE-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Good to see", .after = TERM)
```

#### SIGNAL

``` r
signal <- rank_clean %>% 
  select(contains(c("ID", "SIGNAL")))


signal_final <- signal %>% 
  rename_with(., ~gsub("SIGNAL-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Signal", .after = TERM)
```

#### EMAIL

``` r
email <- rank_clean %>% 
  select(contains(c("ID", "EMAIL")))


email_final <- email %>% 
  rename_with(., ~gsub("EMAIL-", "", .x)) %>% 
  pivot_longer(!`RESP-ID`, names_to = "TERM", values_to = "RANK") %>% 
  na.omit(RANK) %>% 
  group_by(TERM) %>% 
  summarise(mean_rank = mean(RANK)) %>% 
  mutate(SENT = "Email", .after = TERM)
```

#### Combine all the results

``` r
sent_final <- rbind(meeting_final, music_final, goodtosee_final, shutup_final, signal_final, email_final)

sent_final %>% 
  group_by(TERM) %>% 
  summarize(mean = mean(mean_rank))
```

    # A tibble: 4 × 2
      TERM   mean
      <chr> <dbl>
    1 BRO    2.33
    2 BRUH   3.42
    3 DUDE   1.39
    4 GIRL   2.86

``` r
sent_final %>% 
  ggplot(aes(x = TERM, y = mean_rank, group = SENT, col = SENT)) +
  geom_path() +
  geom_point() + 
  scale_y_reverse(limits = c(4, 1))
```

![](AT2023_code_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
  scale_x_discrete(limits = c("DUDE", "BRO", "BRUH", "GIRL"))
```

    <ggproto object: Class ScaleDiscretePosition, ScaleDiscrete, Scale, gg>
        aesthetics: x xmin xmax xend
        axis_order: function
        break_info: function
        break_positions: function
        breaks: waiver
        call: call
        clone: function
        dimension: function
        drop: TRUE
        expand: waiver
        get_breaks: function
        get_breaks_minor: function
        get_labels: function
        get_limits: function
        guide: waiver
        is_discrete: function
        is_empty: function
        labels: waiver
        limits: DUDE BRO BRUH GIRL
        make_sec_title: function
        make_title: function
        map: function
        map_df: function
        n.breaks.cache: NULL
        na.translate: TRUE
        na.value: NA
        name: waiver
        palette: function
        palette.cache: NULL
        position: bottom
        range: <ggproto object: Class RangeDiscrete, Range, gg>
            range: NULL
            reset: function
            train: function
            super:  <ggproto object: Class RangeDiscrete, Range, gg>
        range_c: <ggproto object: Class RangeContinuous, Range, gg>
            range: NULL
            reset: function
            train: function
            super:  <ggproto object: Class RangeContinuous, Range, gg>
        rescale: function
        reset: function
        scale_name: position_d
        train: function
        train_df: function
        transform: function
        transform_df: function
        super:  <ggproto object: Class ScaleDiscretePosition, ScaleDiscrete, Scale, gg>

# *Bro* vs. *Bruh* - Difference

## Data Processing

``` r
library(tidytext)
library(stopwords)
```

    Warning: 패키지 'stopwords'는 R 버전 4.2.2에서 작성되었습니다

``` r
brobruh <- read_csv("data/brobruh.csv")
```

    Rows: 458 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (7): ResponseId, AT ever use, RESP-GENDER, GENDER-4-TEXT, SEXUALITY, AGE...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(brobruh)
```

    # A tibble: 6 × 7
      ResponseId        `AT ever use`     `RESP-GENDER` GENDER…¹ SEXUA…² AGE   Q23  
      <chr>             <chr>             <chr>         <chr>    <chr>   <chr> <chr>
    1 R_9FRUk95l1fWKDZf dude,man          Non-binary    <NA>     Gay     18-25 <NA> 
    2 R_25EnKuXyTReLPE2 dude,girl,man     Feminine      <NA>     Hetero  26-40 Depe…
    3 R_2pLWRKVYVALdO6e bro,dude,girl,man Feminine      <NA>     Bisexu… 26-40 Bruh…
    4 R_PBtJhKNv0JB10GJ bro,dude,man      Masculine     <NA>     Straig… 26-40 Bruh…
    5 R_22wZhXw6DTa6krK dude,man          Masculine     <NA>     Straig… 41-55 <NA> 
    6 R_305B45bvuFeKZUx bro,dude,girl,man Feminine      <NA>     Bisexu… 18-25 I do…
    # … with abbreviated variable names ¹​`GENDER-4-TEXT`, ²​SEXUALITY

``` r
# tokenize the response by sentence, sort out the columns we don't need
brobruh2 <- brobruh %>% 
  rename(ANSWER = Q23) %>% 
  select(-c(`GENDER-4-TEXT`, `AT ever use`)) %>% 
  unnest_tokens(`SENT-TOK`, ANSWER, token = "sentences") %>% 
# need to split the sents by comma, too.
  unnest_tokens(`SENT-TOK2`, `SENT-TOK`, token = stringr::str_split, pattern = ",") %>% 
  na.omit(`SENT-TOK`)
```

``` r
brobruh_clean <- brobruh2 %>% 
  mutate(BRO = grepl('bro', `SENT-TOK2`),
         BRUH = grepl("bruh", `SENT-TOK2`)) %>% 
  relocate(BRO, BRUH, .before = `SENT-TOK2`)

bro_clean <- brobruh_clean %>% 
  filter(BRO == TRUE & BRUH == FALSE)

bruh_clean <- brobruh_clean %>% 
  filter(BRO == FALSE & BRUH == TRUE)


head(bro_clean)
```

    # A tibble: 6 × 7
      ResponseId        `RESP-GENDER` SEXUALITY      AGE   BRO   BRUH  `SENT-TOK2`  
      <chr>             <chr>         <chr>          <chr> <lgl> <lgl> <chr>        
    1 R_305B45bvuFeKZUx Feminine      Bisexual       18-25 TRUE  FALSE " but do use…
    2 R_vjZzVyscBlf29AB Masculine     cis            41-55 TRUE  FALSE "bro is more…
    3 R_Rf6qeJ9SaOibzwJ Feminine      Hetero         18-25 TRUE  FALSE "bro is more…
    4 R_2PofVys50lS49yH Masculine     Straight       26-40 TRUE  FALSE "bro is used…
    5 R_2PofVys50lS49yH Masculine     Straight       26-40 TRUE  FALSE "\"bro"      
    6 R_3g4NXfQGEg4j3Vx Masculine     heteroflexible 26-40 TRUE  FALSE "\"bro\" is …

``` r
head(bruh_clean)
```

    # A tibble: 6 × 7
      ResponseId        `RESP-GENDER` SEXUALITY      AGE   BRO   BRUH  `SENT-TOK2`  
      <chr>             <chr>         <chr>          <chr> <lgl> <lgl> <chr>        
    1 R_2pLWRKVYVALdO6e Feminine      Bisexual queer 26-40 FALSE TRUE  "bruh seems …
    2 R_PBtJhKNv0JB10GJ Masculine     Straight       26-40 FALSE TRUE  "bruh is gen…
    3 R_305B45bvuFeKZUx Feminine      Bisexual       18-25 FALSE TRUE  "i don’t us… 
    4 R_305B45bvuFeKZUx Feminine      Bisexual       18-25 FALSE TRUE  "i think it …
    5 R_vjZzVyscBlf29AB Masculine     cis            41-55 FALSE TRUE  "bruh is mor…
    6 R_1IL6Ojmmkco93Hi Feminine      bi             41-55 FALSE TRUE  "i don't rea…

## Analysis?

``` r
# load up stopwords
snowball <- stopwords(language = "en", source = "snowball", simplify = TRUE)
nltk <- stopwords(language = "en", source = "nltk", simplify = TRUE)
smart <- stopwords(language = "en", source = "smart", simplify = TRUE)

# clean up -- sents containing 'bro'
bro_final <- bro_clean %>% 
  unnest_tokens(TOKEN, `SENT-TOK2`) %>%
  filter(!(TOKEN %in% snowball | TOKEN %in% nltk | TOKEN %in% smart | 
           TOKEN %in% c("bro", "bros", "bruh", "don’t", 'i’m', 'it’s',
                        'people', "person", 'term', "address", "expression", "feel", "feels")))


# clean up -- sents containing 'bruh'
bruh_final <- bruh_clean %>% 
  unnest_tokens(TOKEN, `SENT-TOK2`) %>% 
  filter(!(TOKEN %in% snowball | TOKEN %in% nltk | TOKEN %in% smart | 
           TOKEN %in% c("bro", "bros", "bruh", "don’t", 'i’m', 'it’s',
                        'people', "person", 'term', "address", "expression", "feel", "feels")))
```

### Plot

``` r
# `bro` plot
bro_final %>% 
  count(TOKEN, sort = TRUE, name = "Count") %>% 
  mutate(TOKEN = reorder(TOKEN, Count)) %>% 
  head(20) %>% 
  ggplot(aes(x = Count, y = TOKEN)) +
  geom_col() + 
  labs(title = "Top 20 Tokens Used to Describe 'Bro'")
```

![](AT2023_code_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
#`bruh` plot
bruh_final %>% 
  count(TOKEN, sort = TRUE, name = "Count") %>% 
  mutate(TOKEN = reorder(TOKEN, Count)) %>% 
  head(20) %>% 
  ggplot(aes(x = Count, y = TOKEN)) +
  geom_col() + 
  labs(title = "Top 20 Tokens Used to Describe 'Bruh'")
```

![](AT2023_code_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->
