library(readxl)
library(tidyverse)

congress <- read_csv("congress.csv") #csv�t�@�C���̓ǂݍ���

#��80�c��̃f�[�^�𒊏o���ċL�q���v
cong_80 <- congress %>% filter(congress == 80)
cong_80_rep <- cong_80 %>% filter(party == "Republican")
summary(cong_80_rep$dwnom1)


#��112�c��̃f�[�^�𒊏o���ċL�q���v
cong_112 <- congress %>% filter(congress == 112)
cong_112_rep <- cong_112 %>% filter(party == "Republican")
summary(cong_112_rep$dwnom1)

#���@�C�I�����v���b�g���쐬
cong_80_112_rep <- congress %>% filter(congress == 80 & party == "Republican" | congress == 112 & party == "Republican")
ggplot(cong_80_112_rep, aes(x=factor(congress), dwnom1)) +
  geom_violin() + geom_boxplot(fill = "grey", width=0.1) +
  labs(title = "���a�}�c���̌o�σC�f�I���M�[�̕ϑJ", x="congress", y="eco_ideology")

# 1�Ɠ��l�ɂ܂��Y������P�[�X�������o���ăf�[�^�t���[�����쐬����B�哝�̂����O���邱�ƁB

#��100�c��̋��a�}�c���𒊏o���đ��֌W�������߂�
cong_100_rep <- congress %>% filter(congress == 100 & party == "Republican" & district != 0)
with(cong_100_rep, cor(dwnom1,dwnom2))

#��100�c��̖���}�c���𒊏o���đ��֌W�������߂�
cong_100_dem <- congress %>% filter(congress == 100 & party == "Democrat" & district != 0)
with(cong_100_dem, cor(dwnom1,dwnom2))

# �哝�̂��������o���ĊK�w�N���X�^�[���͂������Ȃ�
# dist�֐��ŃPr�[�X�̗ގ��x(����)�𑪒肷��
# hclust(�P�[�X�̗ގ��x�f�[�^, "���@")
# ����̓E�H�[�h�@���g���̂�ward.D2�Ǝw�肷��

#�哝�̂����o���ăC�f�I���M�[�ϐ��̂ݒ��o
cong_president <- congress %>% filter(district == 0) %>% select(dwnom1,dwnom2)

#�e�P�[�X�̗ގ��x�𑪂�
dis_president <- dist(cong_president)

#�E�H�[�h�@��p����2�������ʂɊK�w�N���X�^�����O�A�f���h���O�����쐬
hc_president <- hclust(dis_president, "ward.D2")
plot(hc_president)
