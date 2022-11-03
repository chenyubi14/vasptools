import numpy as np
import sys

img_dist=float(sys.argv[1])
img_num = img_dist / 0.8
img_num = round(img_num/2)*2 + 1
# img_num in [0, 1) -> 1, [1,3) -> 3, [3, 5) -> 5
# should be an integer images
print(img_num)