import numpy as np

def calculate_triangle_area(p1, p2, p3):
    vec1 = p2 - p1
    vec2 = p3 - p1
    cross_product = np.cross(vec1, vec2)
    return 0.5 * np.linalg.norm(cross_product)

def calculate_orbit_area(positions):
    num_points = positions.shape[0]
    area = 0

    # Loop through the points and form triangles using consecutive points and the first point
    for i in range(1, num_points - 1):
        area += calculate_triangle_area(positions[0], positions[i], positions[i + 1])

    return area

filename='results_orbitoutlines_invAng.out'
data=np.genfromtxt(filename, dtype=str, skip_header=5, invalid_raise=False)
# data looks like [ [kx,ky,kz],[x1,y1,z1],[x2,y2,z2],... [kx,ky,kz],[x1,y1,z1],[x2,y2,z2],..]
sep = np.where(data == ['kx', 'ky', 'kz']) # (array([69, 69, 69]), array([0, 1, 2]))
sep = np.unique(sep[0])

###################
hbar=1.054e-34
echarge=1.6e-19
factor=1e20 # 1/angstrom^2 -> 1/m^2

left=0
# right=0 # no need for this initialization
sep = np.append(sep,len(data)+1 )
#max_b = np.max([0])
for i, sep_i in enumerate(sep): # initial and final is not well considered yet
    # the current order of defining left and right will avoid the judgement of i+1 at the end
    right = sep_i - 1
    orb_i = data[left:right] # [sep[i]+1, sep[i+1]-1]
    orb_i = np.vstack([orb_i,orb_i[0]]) # close the loop
    orb_i = orb_i.astype(float)
    print('orb_i with 2pi\n',orb_i) # I want to have 2pi in the definition
    #orb_i = orb_i / 2 / np.pi # the BZ vectors don't have 2pi
    left = sep_i + 1
    area = calculate_orbit_area(orb_i)
    print('area=',area,'\t 1/angstrom^2')
    freq = hbar*area*factor/2/np.pi/echarge
    print('Frequency=',freq, '\t T')




#positions = np.array([
#    [0, 0, 0],
#    [1, 0, 0],
#    [1, 1, 0],
#    [0, 1, 0],
#    [0, 0, 0]
#    ])
#area = calculate_orbit_area(positions)
#print("Area of the closed orbit in 3D space:", area)
