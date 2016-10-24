samsha <- da36361.0001
cigarettes <- data.frame(samsha[586:588], samsha[681:684], samsha[786:787], samsha[798], samsha[804:807], samsha[809:812])
cigars <- data.frame(samsha[589:591], samsha[813:814])
pipes <- data.frame(samsha[592:593])
alcohol <- data.frame(samsha[606:608], samsha[686:688], samsha[789], samsha[792], samsha[799], samsha[808], samsha[817:818])
marijuana <- data.frame(samsha[609:611], samsha[673:678], samsha[791],samsha[793], samsha[800], samsha[819:820])

risky_avail <- data.frame(samsha[1091:1107], samsha[1086:1090])
dependence <- samsha[1380:1430]
treatment <- samsha[1956:2057]
psych <- samsha[2443:2465]
adult_psychtreat <- samsha[2247:2291]
adult_depressn <- samsha[2549:2577]
alcohol_consumed <- samsha[2960:2989]
demographics <- data.frame(samsha[3062], samsha[3064], samsha[3066], samsha[3072:3075], samsha[3086], samsha[3088], samsha[3137])
colnames(samsha)

