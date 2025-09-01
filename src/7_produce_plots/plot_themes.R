# library(gghighlight)
# colors <- brewer.pal(n = 6, name = 'Set1')
col514 <- c('#78281F','#943126','#B03A2E', '#cb4335','#E74C3C','#ec7063', '#F1948A', '#F5B7B1')
col59 <- c('#7E5109','#9c640c', '#B9770E','#d68910', '#F39C12','#f5b041', '#F8C471', '#FAD7A0')
col6m14y <- c('#154360','#1a5276', '#1F618D','#2471a3', '#2980B9','#5499c7', '#7FB3D5', '#A9CCE3')
col6m4y <- c('#0B5345','#0e6655','#117A65','#138d75','#16A085','#45b39d','#73C6B6','#A2D9CE')#,'#0E6251', '#148F77', '#1ABC9C', '#76D7C4')
col6m2y <- c('#4A235A','#5b2c6f', '#6C3483','#7d3c98', '#8E44AD', '#a569bd','#BB8FCE','#D2B4DE')
col6m9y <- c('#784212','#935116','#AF601A','#ca6f1e','#E67E22', '#eb984e','#F0B27A','#F5CBA7')
colsAB <- c('#186A3B', '#1d8348','#239B56','#28b463', '#2ECC71','#58d68d', '#82E0AA','#ABEBC6')#
colsSVhybrid <- c('#283747', '#85929E')
colors <- c(col514, col59, col6m14y, col6m2y, col6m4y, col6m9y, colsAB, colsSVhybrid, 'black')
colors_effpl <- c(colsAB, col6m2y, col6m4y, col6m9y, col6m14y, col59, col514)

CUcols <- c('#B03A2E','#6C3483','#2471a3','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db')#,'tan')
ABcols <- c('#B03A2E', '#ec407a', '#6a1b9a', '#5c6bc0', '#0288d1','#00796b','#689f38','#ef6c00','#fbc02d')

colsCU <- c(col514[2],col59[3],col6m14y[2],col6m2y[2],col6m4y[2],col6m9y[2])

colscombo <- c('#B03A2E', '#4A235A', '#6C3483', '#A569BD', '#D2B4DE',
                '#0B5345','#117A65','#16A085','#73C6B6',
                '#935116','#CA6F1E','#EB984E', '#F5CBA7',
                '#154360', '#1F618D', '#2980B9', '#7FB3D5',
                '#7E5109', '#B9770E', '#F39C12', '#F8C471',
                '#78281F','#B03A2E', '#E74C3C', '#F1948A')
# 
# CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
# CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')

# CUcols1 <- c(
#   '#440154',  # Dark purple
#   '#482777',  # Purple
#   '#3F4A8A',  # Blue-purple
#   '#31678E',  # Blue
#   '#26838F',  # Blue-green
#   '#1F9D8A',  # Teal
#   '#6CCE5A',  # Green
#   '#B6DE2B',  # Yellow-green
#   '#FEE825',  # Yellow
#   '#283747',  # Black
#   '#808080'   # Gray
# )
# CUcols_ <- c(
#   '#000000',  # Dark purple
#   '#482777',  # Purple
#   '#3F4A8A',  # Blue-purple
#   '#31678E',  # Blue
#   '#26838F',  # Blue-green
#   '#1F9D8A',  # Teal
#   '#6CCE5A',  # Green
#   '#B6DE2B',  # Yellow-green
#   '#FEE825',  # Yellow
#   '#283747',  # Black
#   '#808080'   # Gray
# )
# 
# CUcols1 <- c(
#   '#FEE825',  # Yellow
#   '#B6DE2B',  # Yellow-green
#   '#6CCE5A',  # Green
#   '#1F9D8A',  # Teal
#   '#31678E',  # Blue
#   '#482777',  # Purple
#   '#440154',   # Dark purple
#   '#283747'
# )

# CUcols_ <- c(
#   '#000000',  # Yellow
#   '#B6DE2B',  # Yellow-green
#   '#6CCE5A',  # Green
#   '#1F9D8A',  # Teal
#   '#31678E',  # Blue
#   '#482777',  # Purple
#   '#440154',   # Dark purple
#   '#283747'
# )
CUcols1 <- c(
  "#FDE725", # yellow
  "#B5DE2B", # yellow-green
  "#6CCE59", # green
  "#35B779", # light teal
  "#1F9E89", # teal
  "#31688E", # blue
  "#443983", # violet
  "#440154"  # dark purple
)
CUcols_ <- c(
  "black", 
  "#B5DE2B", # yellow-green
  "#6CCE59", # green
  "#35B779", # light teal
  "#1F9E89", # teal
  "#31688E", # blue
  "#443983", # violet
  "#440154"  # dark purple
)


# CUcols1 <- c(
#   '#E31A1C',  # Bright red (was dark red)
#   '#6A3D9A',  # Purple (adjusted for better contrast)
#   '#1F78B4',  # Blue (slightly adjusted)
#   '#33A02C',  # Green (more distinct from blue)
#   '#FFFF33',  # Bright yellow (was light orange - now more distinct)
#   '#FF7F00',  # Orange (more distinct)
#   '#8B4513',  # Brown (was light green - now very different from yellow)
#   '#A6CEE3',  # Light blue (more distinct from dark blue)
#   '#FB9A99',  # Light pink (was tan)
#   '#283747',  # Black (was dark gray)
#   '#999999'   # Medium gray (better contrast)
# )
# CUcols_ <- c(
#   '#000000',  # Bright red (was dark red)
#   '#6A3D9A',  # Purple (adjusted for better contrast)
#   '#1F78B4',  # Blue (slightly adjusted)
#   '#33A02C',  # Green (more distinct from blue)
#   '#FFFF33',  # Bright yellow (was light orange - now more distinct)
#   '#FF7F00',  # Orange (more distinct)
#   '#8B4513',  # Brown (was light green - now very different from yellow)
#   '#A6CEE3',  # Light blue (more distinct from dark blue)
#   '#FB9A99',  # Light pink (was tan)
#   '#283747',  # Black (was dark gray)
#   '#999999'   # Medium gray (better contrast)
# )
# 
# 
# CUcols1 <- c(
#   '#a6cee3',
#   '#1f78b4',
#   '#b2df8a',
#   '#33a02c',
#   '#fb9a99',
#   '#e31a1c',
#   '#fdbf6f',
#   '#ff7f00',
#   '#cab2d6',
#   '#6a3d9a',
#   '#ffff99'
# )
# CUcols_ <- c(
#   '#000000',
#   '#1f78b4',
#   '#b2df8a',
#   '#33a02c',
#   '#fb9a99',
#   '#e31a1c',
#   '#fdbf6f',
#   '#ff7f00',
#   '#cab2d6',
#   '#283747',
#   '#ffff99'
# )
# CUcols1 <- c(
#   "#E69F00", # orange
#   "#56B4E9", # sky blue
#   "#009E73", # bluish green
#   "#F0E442", # yellow
#   "#0072B2", # blue
#   "#D55E00", # vermillion
#   "#CC79A7", # reddish purple
#   "#00CED1"  # dark turquoise (extra vivid)
# )

# CUcols1 <- c(
#   '#d9f0a3', # darker, easier to see
#   '#c7e9b4',
#   '#7fcdbb',
#   '#41b6c4',
#   '#1d91c0',
#   '#225ea8',
#   '#253494',
#   '#081d58'
# )
# CUcols1 <- c(
#   # '#ffffd9',
#   '#edf8b1',
#   '#c7e9b4',
#   '#7fcdbb',
#   '#41b6c4',
#   '#1d91c0',
#   '#225ea8',
#   '#253494',
#   '#081d58'
# )
# CUcols_ <- c(
#   # '',
#   '#000000',
#   '#c7e9b4',
#   '#7fcdbb',
#   '#41b6c4',
#   '#1d91c0',
#   '#225ea8',
#   '#253494',
#   '#081d58'
# )
