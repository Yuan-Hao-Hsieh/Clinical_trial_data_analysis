---
  header-includes:
  - \usepackage{xeCJK}
- \usepackage{fontspec}
- \setCJKmainfont{微軟正黑體}
- \XeTeXlinebreaklocale "zh"
- \XeTeXlinebreakskip = 0pt plus 1pt
title: "Project - Prospective "
author: 'Group 1'
date: "`r format(Sys.time(), '%d %B %Y')`"
output:
  pdf_document:
  latex_engine: xelatex
toc: yes
toc_depth: '3'
fig_width: 10
fig_height: 6
html_document:
  toc: yes
toc_depth: 3
number_sections: yes
theme: united
highlight: tango
word_document:
  toc: yes
toc_depth: '3'
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



## 輸入資料
原始左眼資料
```{r left data, echo = FALSE}
library(readxl)
left.original <- read_excel('C:/ntpu/3-2/專題/Pro_repeat_left.xlsx')

# setting column names
new_colnames <- c("SIDCD", "VISIT","times","imagedt","complete","clarity","definition","ddaf","daf_2af","ddaf_loc","ddaf_inc_af","ddaf_bck_signal","flecks","ddaf_foci", "ddaf_largest", "ddaf_area","qdaf_well_area","qdaf_poor_area","power", "sensitivity", "focus")
colnames(left.original) <- new_colnames
Eye <- rep("OS",length(left.original$SIDCD))
left.original <- cbind(left.original, Eye)

#compute DAF
DAF <- left.original[c("ddaf_largest","qdaf_well_area","qdaf_poor_area")]
DAF <- rowSums(DAF)
left.original <- cbind(left.original, DAF)

#order VISIT
left.original $VISIT <- factor(left.original $VISIT, levels = c("baseline_visit_arm_2", 
                                                                "6_month_visit_arm_2", "12_month_visit_arm_2", 
                                                                "18_month_visit_arm_2", "24_month_visit_arm_2"))
left.original <- left.original[order(left.original$SIDCD, left.original$VISIT), ]

# function class BaselineGroup
classgroup <- function(DAF, SIDCD, VISIT) {
  # 創建一個空的 dataframe
  E <- data.frame(SIDCD = numeric(), EyeType = character())
  
  # 遍歷每個唯一的 SIDCD
  for (id in unique(SIDCD)) {
    # 檢查是否有任何VISIT ==  "baseline_visit_arm_2"
    baseline_visit_present <- any(VISIT[SIDCD == id] == "baseline_visit_arm_2")
    
    # 獲取該 SIDCD 的 All VISIT
    all_visits <- unique(VISIT[SIDCD == id])
    Baseline <- if (baseline_visit_present && any(DAF[SIDCD == id & VISIT == "baseline_visit_arm_2"] >= 1.9)) {1} else {0}
    df <- data.frame(SIDCD = rep(id, length(all_visits)), Baseline = rep(Baseline, length(all_visits)))
    E <- rbind(E, df)
  }
  return(E)
}

study.class <- classgroup(left.original$DAF, left.original$SIDCD, left.original$VISIT)
left.original <- cbind(left.original, study.class, by = "SIDCD")
left.original <- left.original[,-c(24,26)]
head(left.original, 10)

# left eye
calculate_change_rate.left <- function(DAFLESSIZ, SIDCD) {
  # Set new null dataframe
  change_rate <- data.frame(SIDCD = numeric(), ChangeRate = numeric())
  
  # discuss SIDCD 1:13
  for (id in unique(SIDCD)) {
    # 抓出SIDCD跟DAFLESSIZ對應值
    values <- DAFLESSIZ[SIDCD == id]
    
    # 做SIDCD有兩筆以上的資料
    if (length(values) >= 2) {
      # 多設一個時間點在baseline前的DAFESSIZ設為0
      values <- c(0, values)
      
      # 計算變化率到小數點後第四位
      rates <- round(diff(values) / values[-1], 4)
      
      # 將變化率與SIDCD對應的值合併成dataframe
      rate_df <- data.frame(SIDCD = rep(id, length(rates)), ChangeRate = rates)
      
      # combined ID and change rate
      change_rate <- rbind(change_rate, rate_df)
    }
  }
  
  # check length = 116
  while (nrow(change_rate) < 116) {
    change_rate <- rbind(change_rate, data.frame(SIDCD = 0, ChangeRate = 0))
  }
  return(change_rate)
}

left.result <- calculate_change_rate.left(left.original[["DAF"]],left.original[["SIDCD"]])
left <- cbind(left.original, left.result, by = "SIDCD")
left <- left[, -c(25, 27)]
head(left, 10)
```

原始右眼資料
```{r input right data, echo = FALSE}
# input right eye data
right.original <- read_excel('C:/ntpu/3-2/專題/Pro_repeat_right.xlsx')

# setting column names
new_colnames.right <- c("SIDCD", "VISIT","times","imagedt","ddaf","daf_2af","ddaf_loc","ddaf_inc_af","ddaf_bck_signal","flecks","ddaf_foci", "ddaf_largest", "ddaf_area","qdaf_well_area","qdaf_poor_area","path","power", "sensitivity", "focus")
colnames(right.original) <- new_colnames.right
Eye <- rep("OD",length(right.original$SIDCD))
right.original <- cbind(right.original, Eye)

#compute DAF
DAF <- rowSums(right.original[c("ddaf_largest","qdaf_well_area","qdaf_poor_area")])
right.original <- cbind(right.original, DAF)

#order VISIT
right.original$VISIT <- factor(right.original$VISIT, levels = c("baseline_visit_arm_2", 
                                                                "6_month_visit_arm_2", "12_month_visit_arm_2", 
                                                                "18_month_visit_arm_2", "24_month_visit_arm_2"))
right.original <- right.original[order(right.original$SIDCD, right.original$VISIT), ]

study.class <- classgroup(right.original$DAF, right.original$SIDCD, right.original$VISIT)
right.original <- cbind(right.original, study.class, by = "SIDCD")
right.original <- right.original[,-c(22,24)]
head(right.original, 10)

# right eye
calculate_change_rate.right <- function(DAFLESSIZ, SIDCD) {
  change_rate <- data.frame(SIDCD = numeric(), ChangeRate = numeric())
  for (id in unique(SIDCD)) {
    values <- DAFLESSIZ[SIDCD == id]
    if (length(values) >= 2) {
      values <- c(0, values)
      rates <- round(diff(values) / values[-1], 4)
      rate_df <- data.frame(SIDCD = rep(id, length(rates)), ChangeRate = rates)
      change_rate <- rbind(change_rate, rate_df)
    }
  }
  while (nrow(change_rate) < 107) {
    change_rate <- rbind(change_rate, data.frame(SIDCD = 0, ChangeRate = 0))
  }
  return(change_rate)
}

right.result <- calculate_change_rate.right(right.original[["DAF"]],right.original[["SIDCD"]])
right <- cbind(right.original, right.result, by = "SIDCD")
right <- right[,-c(23,25)]
head(right, 10)
```

## 繪製各變數對DAF在時間下的影響 
```{r compare left and right data, echo = FALSE}
create_plot <- function(data, x_column) {
  # Aggregate data by x_column and VISIT to calculate median
  grouped_data <- aggregate(DAF ~ . + VISIT, data = data[, c(x_column, "DAF", "VISIT")], FUN = median)
  
  # Plot median values
  plot <- ggplot(grouped_data, aes(x = VISIT, y = DAF, color = factor(.data[[x_column]]))) +
    geom_point() +
    geom_line(aes(group = .data[[x_column]])) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Median DAF by", x_column))
  
  return(plot)
}
columns_to_plot <- c("ddaf", "daf_2af", "ddaf_loc", "ddaf_inc_af", "ddaf_bck_signal", "flecks", "ddaf_foci")
library(ggplot2)
library(gridExtra)

plots_list <- list()
for (column in columns_to_plot) {
  
  plot_left <- create_plot(left, column)
  plot_left <- plot_left + ggtitle(paste("Left Eye")) + theme(plot.title = element_text(hjust = 0.5))
  
  plot_right <- create_plot(right, column)
  plot_right <- plot_right + ggtitle(paste("Right Eye")) + theme(plot.title = element_text(hjust = 0.5))
  
  plots_list[[column]] <- list(plot_left, plot_right)
}

for (column in columns_to_plot) {
  grid.arrange(grobs = plots_list[[column]], ncol = 2, top = paste("Median of", column))
}
```


```{r combined data, echo = FALSE}
#delete different columns 
left <- left[ ,-c(5,6,7)]
right <- right[ ,-16]

# select the same SIDCD
common_id <- intersect(left$SIDCD, right$SIDCD)
data1_common <- left[left$SIDCD %in% common_id, ]
data2_common <- right[right$SIDCD %in% common_id, ]
dim(data1_common)
dim(data2_common)

# combined left & right data
pro.data <- rbind(data1_common, data2_common)
pro.data <- pro.data[order(pro.data[["SIDCD"]]), ]

#check length(ID) is the same
length(unique(pro.data$SIDCD))
length(common_id)

compare_eye_group <- function(data) {
  # 選取符合條件的資料
  subset_data <- subset(data, VISIT == "baseline_visit_arm_2")
  
  # 創建一個空的資料框來存放結果
  comparison_result <- data.frame(SIDCD = character(), Eye = character(), Fellow = character(), stringsAsFactors = FALSE)
  
  # 找到每個 SIDCD 的唯一值
  sidcd_values <- unique(subset_data$SIDCD)
  
  # 對每個 SIDCD 進行比較並添加結果到 comparison_result 中
  for (sidcd in sidcd_values) {
    # 選擇特定 SIDCD 的資料
    sidcd_data <- subset(subset_data, SIDCD == sidcd)
    
    # 獲取 OS 和 OD 的 DAF 值
    os_daf <- sidcd_data$DAF[sidcd_data$Eye == "OS"]
    od_daf <- sidcd_data$DAF[sidcd_data$Eye == "OD"]
    
    # 比較 DAF 值，並將結果標記為 Fellow 或 Study
    if (os_daf > od_daf) {
      eye_result <- c("OS" = "Fellow", "OD" = "Study")
    } else {
      eye_result <- c("OS" = "Study", "OD" = "Fellow")
    }
    
    # 將結果添加到 comparison_result 中
    comparison_result <- rbind(comparison_result, data.frame(SIDCD = sidcd, Eye = c("OS", "OD"), Fellow = eye_result, stringsAsFactors = FALSE))
  }
  
  return(comparison_result)
}


baseline_result <- compare_eye_group (pro.data)

apply_baseline_classification <- function(baseline_result, all_data) {
  # 初始化一個空的資料框來存放結果
  result <- data.frame(SIDCD = numeric(), VISIT = character(), Eye = character(), Fellow = character(), stringsAsFactors = FALSE)
  
  # 找到每個 SIDCD 的唯一值
  sidcd_values <- unique(all_data$SIDCD)
  
  # 對每個 SIDCD 進行處理
  for (sidcd in sidcd_values) {
    # 選取特定 SIDCD 的資料
    sidcd_data <- all_data[all_data$SIDCD == sidcd, ]
    
    # 找到該 SIDCD 的所有 VISIT 時間點
    visit_values <- unique(sidcd_data$VISIT)
    
    # 對每個 VISIT 時間點進行處理
    for (visit in visit_values) {
      # 提取特定 SIDCD 和 VISIT 的資料
      visit_data <- sidcd_data[sidcd_data$VISIT == visit, ]
      
      # 提取該 SIDCD 在 baseline 時間點的分組結果
      baseline_sidcd_data <- baseline_result[baseline_result$SIDCD == sidcd, ]
      
      # 確保有對應到 baseline 的分組結果
      if (nrow(baseline_sidcd_data) > 0) {
        # 複製 baseline 的分組結果到該 VISIT 時間點的資料中
        result <- rbind(result, cbind(SIDCD = sidcd, VISIT = visit, Eye = baseline_sidcd_data$Eye, EyeType = baseline_sidcd_data$Fellow))
      }
    }
  }
  
  return(result)
}

Eye.Type <- apply_baseline_classification(baseline_result, pro.data)
pro.data <- cbind(pro.data, Eye.Type)
pro.data <- pro.data[, -c(23:25)]
head(pro.data, 10)
```

## 看原資料間觀察Study Eye和Fellow Eye對時間效果的影響
$$H_0: \mu_{1j} = \mu_{2j}$$
  
  
  ```{r t-test, echo = FALSE}
# 先看組別對時間下的差異，抓取Baseline下的結果
test1 <- t.test(DAF ~ EyeType, subset( pro.data, VISIT == "baseline_visit_arm_2"))
print(test1)
```
-----------------------------------------------------------------------------------------------
  1.做Two smaple T-test，先只看Baseline下組別對時間效果的差異
可以看到在這個檢定，在 95% 信心水準下的信賴區間為(3.9210, 4.022273)不包含0，拒絕虛無假設$H_0$，
推論此Study Eye和Fellow Eye間的平均值在Baseline有顯著差異，接著想看後續時間的效果有沒有影響。



## Compute DAF change rate
DAF Change Rate of Left Eye
```{r delete NA, echo = FALSE}
## check length
sum(pro.data$VISIT == "baseline_visit_arm_2")
length(unique(pro.data$SIDCD))

## Phase II Start to observe
pro.data$ChangeRate[pro.data$VISIT=="baseline_visit_arm_2" ] <- NA
pro.data$ChangeRate[pro.data$SIDCD== "15066" & pro.data$VISIT=="6_month_visit_arm_2"] <- NA

## Delete ChangeRate = "NA"
pro.rate.data  <- pro.data[complete.cases(pro.data),]
head(pro.rate.data, 10)
```

OS - 左
OD - 右
fellow - 嚴重
study - 輕微


## Drawing ChangeRate Plots
Combined
```{r combined ggplot, echo = FALSE}
# see combinded data 
library(ggplot2)
library(gridExtra)

combined.plot <- ggplot(pro.rate.data, aes(VISIT, ChangeRate)) +
  geom_point(aes(color = EyeType)) +
  geom_smooth(aes(group = EyeType, color = EyeType),method = "lm")+
  scale_x_discrete( name = "Timeline")+
  xlab("Timeline")+
  ylab("DAFChangeRate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined.plot2 <- combined.plot + ylim(-0.05, 0.25)
grid.arrange(combined.plot, combined.plot2, nrow = 1)
```

SIDCD scatter plot
```{r scatter SIDCD, echo = FALSE}
## see every SIDCD data  
library(ggplot2)
pro.rate.data$SIDCD <- as.character(pro.rate.data$SIDCD)

sidcd.plot<- ggplot(pro.rate.data, aes(VISIT,ChangeRate)) +
  geom_point(aes(color = SIDCD),alpha = 0.5) +
  xlab("Timeline")+
  ylab("DAFChangeRate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
sidcd.plot
```

## 輸入資料  

```{r od data, include=FALSE}
install.packages("tidyverse")
library(tidyverse)
data <- read_excel("C:/ntpu/3-2/專題/LBS-FAF-toshare1.xlsx")
data$VISIT[data$VISIT == "PHASE 1B END OF STUDY"] <- "PHASE 2 SCREENING/BASELINE"

```

## 處理Study Eye data 並計算變化率
```{r calculate-change-rate-od, echo = FALSE}
data.study <- grepl("Study", data$EyeType)
filtered_data <- data[data.study, ]
#print(filtered_data[['DAFLESSIZ']])

calculate_change_rate <- function(DAFLESSIZ, SIDCD) {
  # Set new null dataframe
  change_rate <- data.frame(SIDCD = numeric(), ChangeRate = numeric())
  
  # discuss SIDCD 1:13
  for (id in unique(SIDCD)) {
    # 抓出SIDCD跟DAFLESSIZ對應值
    values <- DAFLESSIZ[SIDCD == id]
    
    # 做SIDCD有兩筆以上的資料
    if (length(values) >= 2) {
      # 多設一個時間點在baseline前的DAFESSIZ設為0
      values <- c(0, values)
      
      # 計算變化率到小數點後第四位
      rates <- round(diff(values) / values[-1], 4)
      
      # 將變化率與SIDCD對應的值合併成dataframe
      rate_df <- data.frame(SIDCD = rep(id, length(rates)), ChangeRate = rates)
      
      # combined ID and change rate
      change_rate <- rbind(change_rate, rate_df)
    }
  }
  
  # check length = 99
  while (nrow(change_rate) < 99) {
    change_rate <- rbind(change_rate, data.frame(SIDCD = 0, ChangeRate = 0))
  }
  return(change_rate)
}

study.result <- calculate_change_rate(filtered_data[["DAFLESSIZ"]],filtered_data[["SIDCD"]])
merged_data <- cbind(filtered_data, study.result, by = "SIDCD")
new.study.data <- merged_data[, -c(7:14,16,17,18,20,22)]

## Phase II Start to observe
new.study.data $ChangeRate[new.study.data $VISIT=="PHASE 1B SCREENING/BASELINE" & new.study.data $VISIT=="V6 - MONTH 2.5"] <- NA

## Delete ChnageRate = "NA"
new.study.data  <- new.study.data [complete.cases(new.study.data ),]

## set up VISIT order
new.study.data  $VISIT <- factor(new.study.data  $VISIT, levels = c("PHASE 2 SCREENING/BASELINE",
                                                                    "V9 - MONTH 7", 
                                                                    "V13 - MONTH 13", "V15 - MONTH 16", 
                                                                    "V17 - MONTH 19", "V19 - MONTH 22", 
                                                                    "V21 - PHASE 2 END OF TREATMENT"))

# check order
levels(new.study.data $VISIT)
new.study.data  <- new.study.data [order(new.study.data $SIDCD, new.study.data $VISIT), ]
new.study.data  <- new.study.data [complete.cases(new.study.data ),]
head(new.study.data, 10)
```


## 處理Fellow Eye data 並計算變化率
```{r os data, echo = FALSE}
data.fellow <- grepl("Fellow", data$EyeType)
filtered_data2 <- data[data.fellow, ]
#print(filtered_data2[['DAFLESSIZ']])

fellow.result <- calculate_change_rate(filtered_data2[["DAFLESSIZ"]],filtered_data2[["SIDCD"]])
merged_data2 <- cbind(filtered_data2, fellow.result, by = "SIDCD")
new.fellow.data <- merged_data2[, -c(7:14,16,17,18,20,22)]

## Phase II Start to observe
new.fellow.data$ChangeRate[new.fellow.data$VISIT=="PHASE 1B SCREENING/BASELINE"& new.fellow.data $VISIT=="V6 - MONTH 2.5"] <- NA

## Delete ChnageRate = "NA"
new.fellow.data <- new.fellow.data[complete.cases(new.fellow.data),]

new.fellow.data $VISIT <- factor(new.fellow.data $VISIT, levels = c("PHASE 2 SCREENING/BASELINE",
                                                                    "V9 - MONTH 7", 
                                                                    "V13 - MONTH 13", "V15 - MONTH 16", 
                                                                    "V17 - MONTH 19", "V19 - MONTH 22", 
                                                                    "V21 - PHASE 2 END OF TREATMENT"))

# check order
levels(new.fellow.data$VISIT)
new.fellow.data <- new.fellow.data[order(new.fellow.data$SIDCD, new.fellow.data$VISIT), ]
new.fellow.data <- new.fellow.data[complete.cases(new.fellow.data),]
head(new.fellow.data, 10)
```

## merge os.data和od.data
```{r combined LBS data, echo = FALSE}
combined_df <- rbind(new.study.data, new.fellow.data)
combined_df <- combined_df[order(combined_df[["SIDCD"]]), ]
head(combined_df, 10)
```

2.在線性回歸分析中，可以看出除了Baseline下的組別間有顯著差異，其餘時間點的增加減少幅度都很小，
且P-Value 都 > 0.05 ，不拒絕虛無假設$H_0$。

3.由ANOVA表可知，但是兩個變數和交互作用的影響可能不太顯著，需要進一步分析和解釋模型中每個因子的具體影響。

4.$R^2$ = 0.3143，表示此模型只能解釋31 %的變異，解釋力不足。

5.有可能是因為Baseline到六個月後的治療效果有很大的療效，且用藥可能影響的會同時是兩隻眼睛，導致時間效果對於兩個組別是沒有顯著性的。


## 對於線性回歸模型去做預測
```{r pre VS observe, echo = FALSE}
test2 <- lm(DAF ~ VISIT * EyeType, data = pro.data) 
test<- summary(test2)
print(test)

coefficients <- coef(test2)
pro.data$predicted <- predict(test2)

library(ggplot2)
# 繪製散點圖並添加回歸線
ggplot(pro.data, aes(x = predicted, y = DAF, color = EyeType)) +
  geom_point() +
  geom_abline(intercept = coefficients["(Intercept)"], slope = coefficients["VISIT6_month_visit_arm_2"], linetype = "dashed", color = "#FF0000") +
  geom_abline(intercept = coefficients["(Intercept)"] + coefficients["EyeTypeStudy"], slope = coefficients["VISIT6_month_visit_arm_2"] , linetype = "solid", color = "#0000FF") +
  labs(title = "Predicted vs. Observed",
       x = "Predicted DAF",
       y = "Observed DAF")


## 線性回歸分析
test2 <- lm(DAF ~ VISIT * EyeType, data = pro.data) 
test<- summary(test2)
print(test)

plot(residuals(test2))
qqnorm(residuals(test2))
qqline(residuals(test2))

## ANOVA
anova_result <- anova(test2)
anova_table <- as.data.frame(anova_result)
print(anova_table)

library(stargazer)
stargazer(anova_table, type = "html")

```

先簡單的對原始資料做簡易的Model探討時間對於Study Eye和Fellow Eye兩個組別。

## Fitting Linear Model
```{r linear model for ChangeRate, echo = FALSE}
## Two Sample t-test
# 先看組別對時間下的差異
rate.t.test <- t.test(ChangeRate ~ EyeType, subset(pro.rate.data))
print(rate.t.test)

## 線性回歸分析
linear.test <- lm(ChangeRate ~ VISIT * EyeType, data = pro.rate.data) 
linear.sum <- summary(linear.test)
print(linear.sum)

plot(residuals(linear.test))
qqnorm(residuals(linear.test))
qqline(residuals(linear.test))

## ANOVA
linear.test.result <- anova(linear.test)
linear.test.table <- as.data.frame(linear.test.result)
print(linear.test.table)

# Extract model and residual sums of squares
linear.ssr <- sum(linear.test.table$`Sum Sq`[1:3])

linear.sse <- tail(linear.test.table$"Sum Sq", 1)

#R_square
linear.R_square <- linear.ssr / (linear.ssr + linear.sse)
linear.R_square
```
1.做Two smaple T-test，可以看到在這個檢定，在 95%  
信心水準下的信賴區間(-0.04981412, 0.03456948)包含0，不拒絕虛無假設$H_0$，推論此Study Eye和Fellow 
Eye間的平均值與時間效果對於DAF的變化率沒有顯著差異。

2.在線性回歸分析中，可以看出在Baseline時Study跟Fellow的DAF變化率有顯著的差異。

3.由ANOVA表可知，時間、實驗組別和交互作用對於DAF都沒有顯著影響。

4. $R^2$ = 0.0578，表示此模型只能解釋5.78 %的變異，解釋力不足。

接著再繼續探討其他變數 ，Fitting出更好的model。


```{r combine LBS & pro data, each = FALSE}
pros <- pro.rate.data[, c(1, 2, 19, 20, 21, 22, 23)]

# Treatment: 1, Prospective; 0, LBS
pros$Treatment <- 1
pros <- pros[, c("SIDCD", "VISIT", "Eye", "EyeType", "DAF", "Baseline", "ChangeRate", "Treatment")]
levels(pros$VISIT) <- c("baseline_visit_arm_2","Baseline Point", "12_month_visit_arm_2", "18_month_visit_arm_2", "24_month_visit_arm_2")

# 將 "6_month_visit_arm_2" 替換為 "Baseline Point"
pros$VISIT <- replace(pros$VISIT, pros$VISIT == "6_month_visit_arm_2", "Baseline Point")

pros$VISIT[pros$SIDCD == 12008 & pros$VISIT == "12_month_visit_arm_2"] <- "Baseline Point"
head(pros, 10)

LBS <- combined_df[, c(1, 2, 4, 5, 7, 8, 9)]
LBS$Treatment <- 0
colnames(LBS)[which(colnames(LBS) == "DAFBL_condition")] <- "Baseline"
colnames(LBS)[which(colnames(LBS) == "DAFLESSIZ")] <- "DAF"

levels(LBS$VISIT) <- c("Baseline Point", "V9 - MONTH 7",  "V13 - MONTH 13", "V15 - MONTH 16", "V17 - MONTH 19", "V19 - MONTH 22", "V21 - PHASE 2 END OF TREATMENT")
# 將 "PHASE 2 SCREENING/BASELINE" 修改為 "Baseline Point"
LBS$VISIT <- replace(LBS$VISIT, LBS$VISIT == "PHASE 2 SCREENING/BASELINE", "Baseline Point")
head(LBS, 10) 

compare.data <- rbind(pros, LBS)
head(compare.data, 10)

t.test(DAF ~ Treatment, subset(compare.data, VISIT == "Baseline Point"))

t.test(ChangeRate ~ Treatment, subset(compare.data, VISIT == "Baseline Point"))
```


```{r combine LBS & pro, each = FALSE}
install.packages("")
library(MatchIt)

# 建立傾向得分模型
ps_model <- glm(Treatment ~ SIDCD + VISIT + Eye + EyeType + DAF + Baseline + ChangeRate, 
                data = compare.data, family = binomial, 
                control = glm.control(maxit = 100))

# 預測傾向得分
ps_scores <- predict(ps_model, type = "response")

# 將傾向得分添加到 DataFrame
compare.data$PropensityScore <- ps_scores

# 匹配觀察值
matched_data <- matchit(Treatment ~ PropensityScore, data = compare.data, method = "nearest")

# 評估匹配結果
summary(matched_data)

matched_data2 <- match.data(matched_data)

# 進行因果推論分析
matched_model <- lm(ChangeRate  ~ Treatment, data = matched_data2)
summary(matched_model)
```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



