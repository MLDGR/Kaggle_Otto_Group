
localH2O = h2o.init(ip = "localhost",nthreads = -1, port = 54321, startH2O = TRUE, 
                    Xmx = '4g')

train = read.csv('train.csv/train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv/test.csv',header=TRUE,stringsAsFactors = F)
train$class<-as.factor(train$class)
train = train[,-1]
test = test[,-1]
train$
  n_repeat=2
n_size=c(1,90)
n_fold=3
n_tree = 20
balance = T

Caracteristicas <- h2o_feaSelect(train[,-94],as.integer(train[,94]),n_threads = -1,
                                 n_size=c(n_size[1]:n_size[2]),n_repeat=n_repeat,
                                 n_fold = n_fold,n_tree=n_tree,balance=T)

h2o.shutdown(localH2O)

feat_34 42.38621
feat_89 42.26605
feat_74 42.04638
feat_64 40.34605
feat_79 40.25687
feat_56 38.56383
class ~ feat_7 + feat_25 + feat_59 + feat_73 + feat_77 + feat_78 + feat_79 + feat_89


"feat_34+feat_60+feat_67+feat_42+feat_15+feat_11+feat_26+feat_48+feat_40+feat_14+feat_62+feat_43+feat_39+feat_86+feat_24+feat_36+feat_25+feat_32+feat_72+feat_75+feat_68+feat_16+feat_90+feat_9+feat_64+feat_8+feat_59+feat_53+feat_17+feat_50+feat_30+feat_56+feat_1+feat_85+feat_70+feat_71+feat_69+feat_78+feat_88+feat_47+feat_92+feat_76+feat_41+feat_33+feat_79+feat_57+feat_13+feat_38+feat_87+feat_54+feat_37+feat_66+feat_83+feat_29+feat_20+feat_91+feat_77+feat_89+feat_45+feat_58+feat_55+feat_73+feat_35+feat_18+feat_22+feat_74+feat_19+feat_80+feat_27+feat_21+feat_4+feat_10+feat_44+feat_23+feat_3+feat_7+feat_46+feat_52+feat_63+feat_93+feat_2+feat_65"





