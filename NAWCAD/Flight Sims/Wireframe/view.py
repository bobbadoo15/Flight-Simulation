import numpy as np
import json
import math as m
from math import sin, cos, sqrt, asin, atan2, pi, tan
import matplotlib.pyplot as plt
import socket
import struct
import time
import sys

FLOAT_SIZE = 4

# Orientation functions
def euler_2_quat(E):
    E0=0.5*E[0]
    E1=0.5*E[1]
    E2=0.5*E[2]

    Cp=cos(E0)
    Ct=cos(E1)
    Cs=cos(E2)
    Sp=sin(E0)
    St=sin(E1)
    Ss=sin(E2)

    CtCs=Ct*Cs
    StSs=St*Ss
    StCs=St*Cs
    CtSs=Ct*Ss
    return [(Cp*CtCs + Sp*StSs),
            (Sp*CtCs - Cp*StSs),
            (Cp*StCs + Sp*CtSs),
            (Cp*CtSs - Sp*StCs)]

def quat_2_euler(e):
    e0, e1, e2, e3 = e

    condition = e0*e2-e1*e3

    if(abs(condition) == 0.5):
        if (condition == 0.5):
            return [2.0*asin(e1*1.414213562373095),1.57079632679489,0.0]
        else:
            return [2.0*asin(e1*1.4142135623730950),-1.57079632679489,0.0]
    else:
        e02=e0*e0
        e12=e1*e1
        e22=e2*e2
        e32=e3*e3
        return [atan2(2.0*(e0*e1+e2*e3),(e02+e32-e22-e12)),asin(2.0*condition),atan2(2.0*(e0*e3+e1*e2),(e02+e12-e22-e32))]

def body_2_fixed(v,e):
    x, y, z = v
    e0, ex, ey, ez = e
    T0= x*ex + y*ey + z*ez
    T1= x*e0 - y*ez + z*ey
    T2= x*ez + y*e0 - z*ex
    T3= y*ex - x*ey + z*e0
    return [e0*T1 + ex*T0 + ey*T3 - ez*T2,
            e0*T2 - ex*T3 + ey*T0 + ez*T1,
            e0*T3 + ex*T2 - ey*T1 + ez*T0]

def fixed_2_body(v,e):
    x, y, z = v
    e0, ex, ey, ez = e
    T0= -x*ex - y*ey - z*ez
    T1=  x*e0 + y*ez - z*ey
    T2=  y*e0 - x*ez + z*ex
    T3=  x*ey - y*ex + z*e0
    return [e0*T1 - ex*T0 - ey*T3 + ez*T2,
            e0*T2 + ex*T3 - ey*T0 - ez*T1,
            e0*T3 - ex*T2 + ey*T1 - ez*T0]

def quat_norm(e):
    e0, e1, e2, e3 = e
    mag=sqrt(e0*e0+e1*e1+e2*e2+e3*e3)
    return [e0/mag,e1/mag,e2/mag,e3/mag]

# ViewPlane Class
class Camera:
    def __init__(self, json_data):
        # Create camera properties
        self.location = np.array(json_data["location[ft]"])
        euler = json_data["orientation[deg]"]
        euler[0] *= pi/180.0
        euler[1] *= pi/180.0
        euler[2] *= pi/180.0
        self.quat = np.array(euler_2_quat(euler))
        
        self.follow_location = np.array(json_data["follow_location[ft]"])

        self.vp_distance = np.array(json_data["view_plane"]["distance[ft]"])
        self.vp_angle = json_data["view_plane"]["angle[deg]"]*pi/180.0
        self.vp_aspect_ratio = json_data["view_plane"]["aspect_ratio"]

        tempy = self.vp_distance*tan(0.5*self.vp_angle)
        tempz = tempy/self.vp_aspect_ratio

        # body-fixed coordinates
        self.vp_xb = np.array([self.vp_distance, self.vp_distance, self.vp_distance, self.vp_distance])
        self.vp_yb = np.array([-tempy, -tempy, tempy, tempy])
        self.vp_zb = np.array([-tempz, tempz, tempz, -tempz])

        # earth-fixed coordinates
        self.vp_xf = np.array([0.0, 0.0, 0.0, 0.0])
        self.vp_yf = np.array([0.0, 0.0, 0.0, 0.0])
        self.vp_zf = np.array([0.0, 0.0, 0.0, 0.0])
        #  self.vp_xf = np.array([self.location[0]]*4)
        #  self.vp_yf = np.array([self.location[1]]*4)
        #  self.vp_zf = np.array([self.location[2]]*4)
        
        self.lines2D = np.zeros((2,2))

        self.dx =  tempy
        self.dy =  tempz
        
        self.gamma = 0.1 # Adjust this value (e.g., 0.01 to 0.1) for smoother following
        self.follow_increment = 1.0 

    def adjust_responsiveness(self, amount):
        self.gamma = max(0.01, self.gamma + amount)  # Ensure gamma does not go below 0.01
        print(f"Camera responsiveness adjusted to {self.gamma:.2f}")

    def adjust_follow_distance(self, amount):
        self.follow_location[0] += amount
        print(f"Camera follow distance adjusted to {self.follow_location[0]:.2f} ft")

    def set_state(self,location,quat):
        # Modify this so that the camera simply always points at the vehicle origin
        gamma = 0.8 # Adjust this value (e.g., 0.01 to 0.1) for smoother following

        if np.dot(self.quat, quat) < 0.0: # Ensure the quaternions are in the same hemisphere
            quat = -quat

        # Linear interpolation of quaternions
        temp_quat = self.gamma * quat + (1.0 - self.gamma) * self.quat
        self.quat = np.array(quat_norm(temp_quat)) # Normalize the interpolated quaternion

        target_location = location[:] + body_2_fixed(self.follow_location, self.quat)
        self.location = gamma * target_location + (1.0 - gamma) * self.location

        # temp2 = body_2_fixed(self.follow_location,self.quat)
        # self.location[:] = location[:] + temp2[:]

        for i in range(4):
            [self.vp_xf[i],self.vp_yf[i],self.vp_zf[i]] = body_2_fixed([self.vp_xb[i],self.vp_yb[i],self.vp_zb[i]],self.quat)
            self.vp_xf[i] += self.location[0]
            self.vp_yf[i] += self.location[1]
            self.vp_zf[i] += self.location[2]

        p0 = [0.5*(self.vp_xf[0]+self.vp_xf[2]), 0.5*(self.vp_yf[0]+self.vp_yf[2]), 0.5*(self.vp_zf[0]+self.vp_zf[2])]
        p1 = [self.vp_xf[0], self.vp_yf[0], self.vp_zf[0]]
        p2 = [self.vp_xf[1], self.vp_yf[1], self.vp_zf[1]]
        p01 = [p1[0]-p0[0], p1[1]-p0[1], p1[2]-p0[2]]
        p02 = [p2[0]-p0[0], p2[1]-p0[1], p2[2]-p0[2]]
        self.vp_p0 = np.array(p0)
        self.vp_n = np.cross(p01,p02)

class LinesObject:
    def __init__(self,json_data,ax):
        if 'type' in json_data:
            object_type = json_data["type"]
        else:
            object_type = 'vtk' 

        color = json_data["color"]
        self.clipping = False

        if(object_type == 'vtk'):
            vtkfile = json_data["filename"]

            print('\nReading file ',vtkfile)
            with open(vtkfile, 'r') as reader:
                content = reader.readlines()

            points = []
            lines = []

            points_started = False
            lines_started = False

            for line in content:
                if 'POINTS' in line:
                    points_started = True
                    npoints = int(line.split()[1]) # get the number of points
                    continue

                if 'LINES' in line:
                    lines_started = True
                    nlines = int(line.split()[1]) # get the number of lines
                    continue

                if points_started and not lines_started:
                    coords = line.strip().split()
                    if len(coords) == 3: # valid point entry
                        points.append([float(coords[0]), float(coords[1]), float(coords[2])])

                if lines_started:
                    indices = line.strip().split()
                    if len(indices) == 3 and indices[0] == '2': # valid line entry
                        lines.append([int(indices[1]), int(indices[2])])
    
            print("VTK file ",vtkfile," initialized.")

            self.points = np.array(points)
            self.lines = np.array(lines)


        if(object_type == 'grid'):
            print("\nInitializing Grid")
            self.clipping = True
            ground_altitude = json_data["altitude[ft]"]
            grid_number = json_data["grid_number"]
            grid_scale = json_data["grid_scale[ft]"]

            nxlines = grid_number*2 + 1
            self.nlines = 2*nxlines
            self.points = np.zeros((4*nxlines,3))
            self.lines  = np.zeros((2*nxlines,2), dtype=int)
            for i in range(nxlines): # lines aligned with x axis
                self.points[2*i,  :] = [-grid_number*grid_scale, (i-grid_number)*grid_scale, -ground_altitude]
                self.points[2*i+1,:] = [ grid_number*grid_scale, (i-grid_number)*grid_scale, -ground_altitude]
                self.lines[i,0] = 2*i
                self.lines[i,1] = 2*i+1

            for i in range(nxlines): # lines aligned with y axis
                self.points[2*i+2*nxlines  ,:] = [(i-grid_number)*grid_scale, -grid_number*grid_scale, -ground_altitude]
                self.points[2*i+2*nxlines+1,:] = [(i-grid_number)*grid_scale,  grid_number*grid_scale, -ground_altitude]
                self.lines[i+nxlines,0] = 2*i+2*nxlines
                self.lines[i+nxlines,1] = 2*i+2*nxlines+1

        print("LineObject Dimensions: ")
        print("x length [ft] = ",max(self.points[:,0]) - min(self.points[:,0]))
        print("y length [ft] = ",max(self.points[:,1]) - min(self.points[:,1]))
        print("z length [ft] = ",max(self.points[:,2]) - min(self.points[:,2]))

        self.npoints = len(self.points)
        self.nlines = len(self.lines)

        # set up axes for drawing
        self.ax, = ax.plot([],[],marker='',ls='-',color=color)

        # allocate space for 3D points in earth-fixed coordinates
        self.points3D = np.zeros((self.npoints,3))
        self.set_state([0.0, 0.0, 0.0], [1.0, 0.0, 0.0, 0.0])

        # print(self.points3D)
        # print(self.lines)
        # allocate space for 2D points on viewplane
        self.points2D = np.zeros((self.npoints,2)) # only x and y components in view plane coords
        self.pba = np.zeros((self.npoints,3))
        self.t = np.zeros(self.npoints)
        self.pf = np.zeros((self.npoints,3))

        self.lines2D = np.full((self.nlines*3,2), None, dtype=object) # fills array with "None"

        # print("length of linesN2D = ",len(self.lines2D))
        # print(self.lines2D)


    def draw(self, camera):
        # get all the points in the 2D viewing plane
        pa = camera.location
        p0a = pa[:] - camera.vp_p0[:]
        temp = np.dot(camera.vp_n,p0a)

        self.pba[:] = pa - self.points3D

        self.t[:] = temp/np.dot(self.pba,camera.vp_n)

        self.pf[:] = (self.points3D - pa)*self.t[:, None]

        e0, ex, ey, ez = camera.quat
        a00 = ex*ex + e0*e0 - ey*ey - ez*ez
        a01 = 2*(ex*ey - ez*e0)
        a02 = 2*(ex*ez + ey*e0)
        a10 = 2*(ex*ey + ez*e0)
        a11 = ey*ey + e0*e0 - ex*ex - ez*ez
        a12 = 2*(ey*ez - ex*e0)
        a20 = 2*(ex*ez - ey*e0)
        a21 = 2*(ey*ez + ex*e0)
        a22 = ez*ez + e0*e0 - ex*ex - ey*ey

        self.points2D[:,0] =   a01*self.pf[:,0] + a11*self.pf[:,1] + a21*self.pf[:,2]
        self.points2D[:,1] = - a02*self.pf[:,0] - a12*self.pf[:,1] - a22*self.pf[:,2]

        # This code could possibly be sped up by using intrinsic loops in numpy or something
        for i in range(self.nlines):
            i0 = self.lines[i,0]
            i1 = self.lines[i,1]

            if(self.t[i0]>0 and self.t[i1]>0): # entire line is in front of view plane
                self.lines2D[3*i,  :] = self.points2D[i0,:]
                self.lines2D[3*i+1,:] = self.points2D[i1,:]
            elif(self.t[i0]<0 and self.t[i1]<0): # entire line is behind view plane
                self.lines2D[3*i,  :] = [None,None]
                self.lines2D[3*i+1,:] = [None,None]
            elif(self.clipping): # one point is in front and one is behind the view plane
                qa = self.points3D[i0,:]
                pb = self.points3D[i1,:]
                q0a = qa[:] - camera.vp_p0[:]
                temp2 = np.dot(camera.vp_n,q0a)
                pba = [qa[0]-pb[0], qa[1]-pb[1], qa[2]-pb[2]]
                t = temp2/np.dot(pba,camera.vp_n)
                pf = qa[:] + (pb[:] - qa[:])*t - camera.location[:]
                pointb = fixed_2_body(pf,camera.quat)
                # set points to default value
                self.lines2D[3*i,  :] = self.points2D[i0,:]
                self.lines2D[3*i+1,:] = self.points2D[i1,:]
                #overwrite point behind viewplane with point in plane of viewplane
                if(self.t[i0]<0): #first point is behind plane
                    self.lines2D[3*i,  :] = [pointb[1], -pointb[2]]
                if(self.t[i1]<0): #second point is behind plane
                    self.lines2D[3*i+1,:] = [pointb[1], -pointb[2]]

        self.ax.set_data(self.lines2D[:,0], self.lines2D[:,1])


    def set_state(self, location, quat):
        # this is two orders of magnitude faster than the one below
        e0, ex, ey, ez = quat
        a00 = ex*ex + e0*e0 - ey*ey - ez*ez
        a01 = 2*(ex*ey - ez*e0)
        a02 = 2*(ex*ez + ey*e0)
        a10 = 2*(ex*ey + ez*e0)
        a11 = ey*ey + e0*e0 - ex*ex - ez*ez
        a12 = 2*(ey*ez - ex*e0)
        a20 = 2*(ex*ez - ey*e0)
        a21 = 2*(ey*ez + ex*e0)
        a22 = ez*ez + e0*e0 - ex*ex - ey*ey

        self.points3D[:,0] = a00*self.points[:,0] + a01*self.points[:,1] + a02*self.points[:,2] + location[0]
        self.points3D[:,1] = a10*self.points[:,0] + a11*self.points[:,1] + a12*self.points[:,2] + location[1]
        self.points3D[:,2] = a20*self.points[:,0] + a21*self.points[:,1] + a22*self.points[:,2] + location[2]

        # alternate way (slower)
        # for i in range(self.npoints):
        #     self.points3D[i,:] = body_2_fixed(self.points[i,:],quat)
        #     self.points3D[i,:] += location[:]

class TickerTape:
    def __init__(self, ax, color, direction, total_length, num_ticks, delta_tick_value, tick_location, tick_length, display_text=False, text_x_offset=0.0, text_y_offset=0.0):
        self.direction = direction
        self.total_length = total_length
        self.num_ticks = num_ticks
        self.delta_tick_value = delta_tick_value
        self.tick_location = tick_location
        self.tick_length = tick_length
        self.display_text = display_text
        self.text_x_offset = text_x_offset
        self.text_y_offset = text_y_offset

        self.dl = total_length/float(num_ticks)
        self.ticks = np.full((2*3*num_ticks,2), None, dtype=object)
        self.ax, = ax.plot([],[],marker='',ls='-',color=color)

        if(self.direction == 'vertical'):
            self.axis0 = 0
            self.axis1 = 1
        else:
            self.axis0 = 1
            self.axis1 = 0

        if(self.display_text):
            self.text_location = np.zeros((2*num_ticks,2))
            self.text = np.zeros((2*num_ticks))
            self.text_ax = []
            for i in range(2*num_ticks):
                axi = ax.text(0.0, 0.0, str("{:0.0f}".format(0.0)), color=color)
                self.text_ax.append(axi)

    def update(self,value,heading_limit=False):
        offset = self.dl-self.dl*(value - (value//self.delta_tick_value)*self.delta_tick_value)/self.delta_tick_value
        for i in range(2*self.num_ticks):
            self.ticks[3*i  ,:] = [ self.tick_location,                    offset + (i-self.num_ticks)*self.dl]
            self.ticks[3*i+1,:] = [ self.tick_location + self.tick_length, offset + (i-self.num_ticks)*self.dl]
        self.ax.set_data(self.ticks[:,self.axis0], self.ticks[:,self.axis1])

        if(self.display_text):
            # offset = self.dl-self.dl*(value - (value//self.dv)*self.dv)/self.dv
            for i in range(2*self.num_ticks):
                self.text_location[i] = [self.tick_location + self.text_x_offset, offset + (i-self.num_ticks)*self.dl + self.text_y_offset]
                self.text[i] = (value//self.delta_tick_value)*self.delta_tick_value - float(self.num_ticks-i-1)*self.delta_tick_value
                if(heading_limit):
                    if(self.text[i] < 0.0):
                        self.text[i] += 360.0
                    if(self.text[i] >= 360.0):
                        self.text[i] -= 360.0
                self.text_ax[i].set_text(str("{:0.0f}".format(self.text[i])))
                self.text_ax[i].set_position((self.text_location[i,self.axis0], self.text_location[i,self.axis1]))


class HUD:
    def __init__(self,json_data,ax,camera):
        color = json_data["color"]
        box_background_color = 'grey'
        dx = camera.dx
        dy = camera.dy

        # Ticker Tapes

        # Elevation
        self.ax_elevation_ticker, = ax.plot([],[],marker='',ls='--',color=color)
        self.elevation_ticker = np.full((8,2), None, dtype=object) # fills array with "None"

        # Altitude
        self.altitude_minor = TickerTape(ax,color,'vertical',0.4*dy,10, 100, 0.40*dx, -0.02*dx)
        self.altitude_major = TickerTape(ax,color,'vertical',0.4*dy,1 , 1000,0.40*dx, -0.05*dx,True,0.01*dx,-0.02*dy)
        ax.fill([0.4*dx, 0.42*dx, 0.5*dx, 0.5*dx, 0.42*dx, 0.4*dx],[0.0, 0.05*dy, 0.05*dy, -0.05*dy, -0.05*dy, 0.0],facecolor=box_background_color,edgecolor=color,linewidth=1,zorder=100)
        self.altitude_box = ax.text(0.415*dx, -0.02*dy, str("{:0.0f}".format(0.0)), color=color,zorder=101)

        # Velocity
        self.velocity_minor = TickerTape(ax,color,'vertical',0.4*dy,10, 10, -0.40*dx, 0.02*dx)
        self.velocity_major = TickerTape(ax,color,'vertical',0.4*dy,1 , 100,-0.40*dx, 0.05*dx,True,-0.08*dx,-0.02*dy)
        ax.fill([-0.4*dx, -0.42*dx, -0.5*dx, -0.5*dx, -0.42*dx, -0.4*dx],[0.0, 0.05*dy, 0.05*dy, -0.05*dy, -0.05*dy, 0.0],facecolor=box_background_color,edgecolor=color,linewidth=1,zorder=100)
        self.velocity_box = ax.text(-0.49*dx, -0.02*dy, str("{:0.0f}".format(0.0)), color=color,zorder=101)

        # Heading
        self.heading_minor = TickerTape(ax,color,'horizontal',0.2*dx,4, 5, -0.48*dy, 0.02*dy)
        self.heading_major = TickerTape(ax,color,'horizontal',0.2*dx,2, 10,-0.48*dy, 0.05*dy,True,-0.03*dx,-0.03*dy)
        ax.fill([0.0, -0.04*dx, -0.04*dx, 0.04*dx, 0.04*dx, 0.0],[-0.48*dy, -0.5*dy, -0.57*dy, -0.57*dy, -0.5*dy, -0.48*dy],facecolor=box_background_color,edgecolor=color,linewidth=1,zorder=100)
        self.heading_box = ax.text(-0.03*dx, -0.55*dy, str("{:0.0f}".format(0.0)), color=color,zorder=101)

        # Mach
        ax.text(0.4*dx, -0.45*dy, 'M', color=color)
        ax.fill([0.43*dx, 0.5*dx, 0.5*dx, 0.43*dx],[-0.46*dy, -0.46*dy, -0.39*dy, -0.39*dy],facecolor=box_background_color,edgecolor=color,linewidth=1,zorder=100)
        self.Mach_box = ax.text(0.435*dx, -0.45*dy, str("{:1.2f}".format(0.0)), color=color,zorder=101)

    def draw(self, camera, state):
        dx = camera.dx
        dy = camera.dy
        # Elevation Ticker
        center = state.eul[1]*180.0/pi//10
        self.elevation_ticker[0,:] = [-0.2*dx, -sin(state.eul[1]+(10.0+center)*pi/180.0)]
        self.elevation_ticker[1,:] = [ 0.2*dx, -sin(state.eul[1]+(10.0+center)*pi/180.0)]

        self.elevation_ticker[3,:] = [-0.2*dx, -sin(state.eul[1])]
        self.elevation_ticker[4,:] = [ 0.2*dx, -sin(state.eul[1])]

        self.elevation_ticker[6,:] = [-0.2*dx, -sin(state.eul[1]-(10.0+center)*pi/180.0)]
        self.elevation_ticker[7,:] = [ 0.2*dx, -sin(state.eul[1]-(10.0+center)*pi/180.0)]
        self.ax_elevation_ticker.set_data(self.elevation_ticker[:,0], self.elevation_ticker[:,1])

        # Altitude Ticker
        self.altitude_minor.update(-state.location[2])
        self.altitude_major.update(-state.location[2])
        self.altitude_box.set_text(str("{:0.0f}".format(-state.location[2])))

        # Velocity Ticker
        self.velocity_minor.update(state.V)
        self.velocity_major.update(state.V)
        self.velocity_box.set_text(str("{:0.0f}".format(state.V)))

        # Heading Ticker
        self.heading_minor.update(state.eul[2]*180.0/pi)
        self.heading_major.update(state.eul[2]*180.0/pi,True)
        self.heading_box.set_text(str("{:0.0f}".format(state.eul[2]*180.0/pi)))

        # Mach
        # print(state.Mach)
        self.Mach_box.set_text(str("{:1.2f}".format(state.Mach)))


class UDP:
    def __init__(self,json_data):
        # Set up the socket
        self.ip = "0.0.0.0"  # Listen on all available network interfaces
        self.port = json_data["port_ID"]     # Port to listen on

        # Create a UDP socket
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        # Bind the socket to the IP and port
        self.sock.bind((self.ip, self.port))
        self.blocking = False
        self.sock.setblocking(self.blocking)

        self.time = 0.0
        self.u = 0.0
        self.v = 0.0
        self.w = 0.0
        self.V = 0.0
        self.ax = 0.0
        self.ay = 0.0
        self.az = 0.0
        self.location = np.array([0.0, 0.0, 0.0])
        self.quat = np.array([1.0, 0.0, 0.0, 0.0])
        self.eul = np.array([0.0, 0.0, 0.0])
        self.Mach = 0.0
        self.ground_altitude = 0.0
        self.controls = np.array([0.0, 0.0, 0.0, 0.0])

        print(f"\nListening for UDP packets on {self.ip}:{self.port}")

    def update(self):
        # print('trying to read')
        data_received = False
        try:
            
            # flush the socket buffer
            while True:
                data, addr = self.sock.recvfrom(1024)  # Buffer size is 1024 bytes

                # if we received data process it
                if data:
                    data_received = True
                    num_floats = len(data) // FLOAT_SIZE
                    floats = struct.unpack('<' + 'f' * num_floats, data)

                    # update all state variables
                    self.time = floats[0]
                    self.u = floats[1]
                    self.v = floats[2]
                    self.w = floats[3]
                    self.V = sqrt(self.u*self.u + self.v*self.v + self.w*self.w)
                    
                    self.p = floats[4]
                    self.q = floats[5]
                    self.r = floats[6]

                    self.ax = 0.0
                    self.ay = 0.0
                    self.az = 0.0
                    self.location[0] = floats[7]
                    self.location[1] = floats[8]
                    self.location[2] = floats[9]

                    self.quat[0] = floats[10]
                    self.quat[1] = floats[11]
                    self.quat[2] = floats[12]
                    self.quat[3] = floats[13]

                    # print(f"Updated location: {self.location}, quaternion: {self.quat}") # Add this line

                    self.eul = quat_2_euler(self.quat)
                    if(self.eul[2] < 0.0):
                        self.eul[2] += 2*pi
                    self.Mach = self.V/1100.0 # assuming 1100 ft/s is the speed of sound
                    self.ground_altitude = -self.location[2] + 4500.0 # assuming 4600 ft is the ground altitude

                    # Print the received float values
                    # print("Received floats:", floats)
        except BlockingIOError:
            pass
        except Exception as e:
            print(f"Error during UDP update: {e}")
        return data_received

class SmoothState:
    def __init__(self, alpha=0.1):
        self.alpha = alpha
        self.location = None
        self.quat = None

    def update(self, new_location, new_quat):
        if self.location is None:
            self.location = np.array(new_location)
        else:
            self.location = self.alpha * np.array(new_location) + (1 - self.alpha) * self.location

        if self.quat is None:
            self.quat = np.array(new_quat)
        else:
            # Slerp or simple normalized lerp:
            if np.dot(self.quat, new_quat) < 0.0:
                new_quat = -np.array(new_quat)
            blended = self.alpha * np.array(new_quat) + (1 - self.alpha) * self.quat
            self.quat = blended / np.linalg.norm(blended)

    def get_smoothed(self):
        return self.location, self.quat



def on_key(event):
    if event.key == 'up':
        camera.adjust_follow_distance(camera.follow_increment) # Move closer
    elif event.key == 'down':
        camera.adjust_follow_distance(-camera.follow_increment) # Move farther
    elif event.key == 'right':
        camera.adjust_responsiveness(0.01) # Increase responsiveness
    elif event.key == 'left':
        camera.adjust_responsiveness(-0.01) # Decrease responsiveness

# Read in settings
with open(sys.argv[1], 'r') as file:
    data = json.load(file)

# Create view plane in local coordinate system
camera = Camera(data["camera"])

# Create Figure
fig = plt.figure(figsize=(camera.vp_aspect_ratio*5.0,5.0))
ax = fig.add_subplot(111)
plt.subplots_adjust(top=1.0, bottom=0.0, left=0.0, right=1.0)
# plt.subplots_adjust(top=0.99, bottom=0.01, left=0.001, right=0.999)
fig.canvas.mpl_connect('key_press_event', on_key)
plt.axis('off')
ax.axes.set_xlim( camera.vp_yb[0], camera.vp_yb[2])
ax.axes.set_ylim(-camera.vp_zb[1],-camera.vp_zb[0])
ax.axes.xaxis.set_ticklabels([])
ax.axes.yaxis.set_ticklabels([])
ax.set_xticks([])
ax.set_yticks([])
ax.axes.set_aspect('equal')
fig.canvas.draw()
plt.show(block=False)

smooth_state = SmoothState(alpha=0.1)  # Initialize the smooth state with a default alpha

# Create Ground Plane
show_ground = False
if("ground" in data["scene"]):
    show_ground = True
    ground = LinesObject(data["scene"]["ground"],ax)
    ground_altitude = ground.points[0,2]
    #  input(f'ground altitude: {ground_altitude}')
    ground_scale = data["scene"]["ground"]["grid_scale[ft]"]

# Create Scenery
show_scenery = False
if("scenery" in data["scene"]):
    show_scenery = True
    scenery = LinesObject(data["scene"]["scenery"],ax)

# Create Vehicle
show_vehicle = False
if("vehicle" in data):
    show_vehicle = True
    vehicle = LinesObject(data["vehicle"],ax)

# Create HUD
show_hud = False
if("hud" in data):
    show_hud = True
    hud = HUD(data["hud"],ax,camera)

# Create connections
connection = UDP(data["connections"]["receive_states"])

# === [START] FINAL, STABLE MAIN LOOP =========================================

# --- Main Loop Setup ---
TARGET_FPS = 60.0
FRAME_TIME = 1.0 / TARGET_FPS

# Create HUD text objects
t_time = ax.text(camera.vp_yb[0], camera.vp_zb[0] + 0.05, ' time [s] = ' + str("{:.2f}".format(connection.time)))
t_fps  = ax.text(camera.vp_yb[0], camera.vp_zb[0] + 0.02, '      fps = ' + str("{:.2f}".format(TARGET_FPS)))

# --- Initial State Setup ---
# Initialize the camera and vehicle positions before the loop starts
#  camera.set_state(connection.location, connection.quat)
camera.set_state(camera.location, camera.quat)
if show_vehicle:
    #  vehicle.set_state(connection.location, connection.quat)
    vehicle.set_state(camera.location, camera.quat)

connection.location = camera.location
connection.quat = camera.quat

print("\n--- Viewer is running ---")
print("Controls: [Up/Down Arrows] = Adjust Distance, [Left/Right Arrows] = Adjust Responsiveness")

# Loop until the user closes the window
while plt.fignum_exists(fig.number):
    time_begin = time.time()

    # 1. GET DATA FIRST: Always check for new data at the start of the frame.
    # The connection object's state (location, quat) is updated here if a packet is received.
    connection.update()

    # 2. UPDATE ALL STATES: Now, update all objects based on the latest connection state.
    # This ensures the camera and vehicle are always in sync.
    smooth_state.update(connection.location, connection.quat)
    location, quat = smooth_state.get_smoothed()
    camera.set_state(location, quat)
    
    if show_vehicle:
        vehicle.set_state(location, quat)

    # 3. DRAW SCENE ONCE: With all states updated, draw every visible component.
    if show_ground:
        #  print([int(location[0]/ground_scale)*ground_scale, int(location[1]/ground_scale)*ground_scale,ground_altitude])
        ground.set_state([int(location[0]/ground_scale)*ground_scale, int(location[1]/ground_scale)*ground_scale,0.], [1.,0.,0.,0.])
        ground.draw(camera)
    
    if show_vehicle:
        vehicle.draw(camera)

    if show_hud:
        hud.draw(camera, connection)

    # Update HUD text displays
    t_time.set_text(' time[s] = ' + str("{:.2f}".format(connection.time)))
    t_fps.set_text('      fps = ' + str("{:.2f}".format(1.0 / FRAME_TIME))) # Display target FPS

    # 4. MANAGE FRAME RATE: Draw the canvas and pause to maintain a steady FPS.
    fig.canvas.draw()
    fig.canvas.flush_events()

    elapsed_time = time.time() - time_begin
    sleep_time = FRAME_TIME - elapsed_time
    if sleep_time > 0:
        time.sleep(sleep_time)

print("--- Viewer window closed ---")

# === [END] FINAL, STABLE MAIN LOOP ===========================================



