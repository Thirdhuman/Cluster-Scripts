# Continuous RUCA

ruca=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling Regions/Colleges & Growth/Raw/ruca2010revised.xlsx")
# ruca$Primary_RUCA = ifelse( ruca$Primary_RUCA == 99, NA, Primary_RUCA))
# 
# ruca$Primary_RUCA = ifelse( ruca$Primary_RUCA == 99, NA, Primary_RUCA))

# 
# Code	Classification description
# 1 Metropolitan area core: primary flow within an urbanized area (UA)
# 1.0	No additional code
# 1.1	Secondary flow 30% to 50% to a larger UA
# 2 Metropolitan area high commuting: primary flow 30% or more to a UA
# 2.0	No additional code
# 2.1	Secondary flow 30% to 50% to a larger UA
# 3 Metropolitan area low commuting: primary flow 10% to 30% to a UA
# 3.0	No additional code
# 4 Micropolitan area core: primary flow within an urban cluster of 10,000 to 49,999 (large UC)
# 4.0	No additional code
# 4.1	Secondary flow 30% to 50% to a UA
# 5 Micropolitan high commuting: primary flow 30% or more to a large UC
# 5.0	No additional code
# 5.1	Secondary flow 30% to 50% to a UA
# 6 Micropolitan low commuting: primary flow 10% to 30% to a large UC
# 6.0	No additional code
# 7 Small town core: primary flow within an urban cluster of 2,500 to 9,999 (small UC)
# 7.0	No additional code
# 7.1	Secondary flow 30% to 50% to a UA
# 7.2	Secondary flow 30% to 50% to a large UC
# 8 Small town high commuting: primary flow 30% or more to a small UC
# 8.0	No additional code
# 8.1	Secondary flow 30% to 50% to a UA
# 8.2	Secondary flow 30% to 50% to a large UC
# 9 Small town low commuting: primary flow 10% to 30% to a small UC
# 9.0	No additional code
# 10 Rural areas: primary flow to a tract outside a UA or UC
# 10.0	No additional code
# 10.1	Secondary flow 30% to 50% to a UA
# 10.2	Secondary flow 30% to 50% to a large UC
# 10.3	Secondary flow 30% to 50% to a small UC
# 99 Not coded: Census tract has zero population and no rural-urban identifier information



