###--------------------------------------------------
### Sex Composition of New Zealand Electorates
### Kieran Healy
### @kjhealy
###--------------------------------------------------

library(ggplot2)
library(tidyr)

### Data from Jonathan Marshall
### https://github.com/jmarshallnz/electorates

dat <- read.csv("data/electorates.csv",
                  row.names = 1)

dat.w <- spread(dat, Sex, count)
dat.w$Pop <- dat.w$Male + dat.w$Female
dat.w$Diff <- dat.w$Male / dat.w$Female
dat.w$pMale <- (dat.w$Male /  (dat.w$Male + dat.w$Female))
dat.w$pFemale <- (dat.w$Female /  (dat.w$Male + dat.w$Female))
dat.w$pDiff <- dat.w$pMale - dat.w$pFemale

dat.w$pos <- dat.w$Diff > 1

dat.w$Electorate <- reorder(dat.w$Electorate, dat.w$Diff, order=TRUE)

pct.labs <- c("0.6", "0.8", "1", "1.2")

age.lab <- c("0-4", "5-9", "10-14", "15-19", "20-24",
             "25-29", "30-34", "35-39", "40-44", "45-49",
             "50-54", "55-59", "60-64", "65-69", "70-74",
             "75-79", "80-84", "85 and up")

age.levels <- c("0–4 Years", "5–9 Years", "10–14 Years",
                "15–19 Years", "20–24 Years",
                "25–29 Years", "30–34 Years",
                "35–39 Years", "40–44 Years",
                "45–49 Years", "50–54 Years",
                "55–59 Years", "60–64 Years",
                "65–69 Years", "70–74 Years",
                "75–79 Years", "80–84 Years",
                "85 Years And Over")

dat.w$Age <- factor(dat.w$Age, levels = age.levels, ordered = TRUE)

p0 <- ggplot(dat.w, aes(x = Age,
                        ymax = Diff,
                        ymin = 1,
                        group = Electorate,
                        color = pos))

p1 <- p0 + geom_linerange(size=1.2) +
    labs(x="",
         y="Ratio of Male to Female Population",
         color="Sex Composition") +
    scale_color_manual(labels = c("Majority Female", "Majority Male"),
                       values=c("#E69F00", "#0072B2")) +
    scale_x_discrete(labels=age.lab) +
    scale_y_continuous(breaks=c(0.6, 0.8, 1, 1.2), labels=pct.labs) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6),
          axis.text.x = element_text(size = 6)) +
    theme(legend.position="top") + facet_wrap(~ Electorate, ncol = 4)

cairo_pdf(file="figures/nz-surplus-males.pdf", height=30, width=6)
print(p1)
dev.off()


ggsave(
    "figures/nz-surplus-males.png",
    p1,
    width=6,
    height=30,
    dpi=300
    )
