install.packages("WDI")
library("WDI")
# سحب البيانات لمصر من 1980 لـ 2022
egy_data <- WDI(country = "EG", 
                indicator = c("growth" = "NY.GDP.MKTP.KD.ZG", 
                              "gov_exp" = "NE.CON.GOVT.ZS"), 
                            start = 1980, end = 2022)
View(egy_data)
# معرفة عدد القيم المفقودة في كل متغير
summary(egy_data)
# ترتيب البيانات حسب السنة وحذف الأعمدة غير الضرورية
library(tidyverse)
egy_clean <- egy_data %>%
  select(year, growth, gov_exp) %>%
  arrange(year) %>%
  drop_na()
# حساب معامل ارتباط بيرسون
correlation_value <- cor(egy_clean$growth, egy_clean$gov_exp)
print(correlation_value)
# رسم العلاقة بين الإنفاق الحكومي والنمو
ggplot(egy_clean, aes(x = gov_exp, y = growth)) +
  geom_point(color = "steelblue", size = 3) + # رسم النقاط
  geom_smooth(method = "lm", color = "red") + # رسم خط الاتجاه (Linear Model)
  labs(title = "العلاقة بين الإنفاق الحكومي والنمو الاقتصادي في مصر",
       x = "الاستهلاك الحكومي (% من الناتج المحلي)",
       y = "معدل النمو السنوي %") +
  theme_minimal()
# بناء النموذج وحفظه في متغير
final_model <- lm(growth ~ gov_exp, data = egy_clean)

# عرض النتائج التفصيلية
summary(final_model)
# إضافة التوقعات للبيانات
egy_clean$predicted <- predict(final_model)

# رسم المقارنة
ggplot(egy_clean, aes(x = year)) +
  geom_line(aes(y = growth, color = "النمو الفعلي"), size = 1) +
  geom_line(aes(y = predicted, color = "توقعات النموذج"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("النمو الفعلي" = "steelblue", "توقعات النموذج" = "red")) +
  labs(title = "مدى دقة النموذج في تتبع النمو الاقتصادي المصري",
       subtitle = "مقارنة بين الواقع التاريخي وتوقعات الانحدار الخطي",
       x = "السنة", y = "معدل النمو %", color = "الدليل") +
  theme_minimal()