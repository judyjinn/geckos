'''
geckos.py 
Graphing and statistically analyzing gecko water walking
created: April 12, 2017
last updated: Aug 21, 2017
Judy Jinn
'''

'''                         #######################
#------------------------   ## ---   Set up  --- ##     ------------------------
                            #######################
'''
"""import every package you'll need"""
import os 
import re #used for natural sort function

import pandas as pd  #data frames
import math

import matplotlib
matplotlib.use("TkAgg") # Need to do this for tkinter to properly import and allow simulataneous matplotlib figures
from matplotlib import pyplot as plt
import matplotlib.animation as animation #animate plots, unused
import matplotlib.cm as cm
import matplotlib.patches as patches
import tkinter as Tk #opening files
from tkinter import filedialog
import numpy as np
import scipy.stats as stats
import scipy.io as sio
import scipy.stats as stats

# auto-detect width of terminal for displaying dataframes
pd.set_option('display.max_columns',0) 
plt.ion() # Solves that matplotlib hang problem
pd.options.mode.chained_assignment = None 


'''                                         #######################
#----------------------------------------   ## ---    MAIN   --- ##     -----------------------------------------
                                            #######################
'''

if __name__ == '__main__':
    
    geckos_csv = pd.read_csv("/Users/judyjinn/Python/gecko_water/soapwatertrunc.csv")
    
    # multiply all leg forces by 1000 to get mN    
    geckos_csv['fl.slap.force'] = geckos_csv['fl.slap.force']*1000
    geckos_csv['fl.stroke.ver.force'] = geckos_csv['fl.stroke.ver.force']*1000
    geckos_csv['fl.stroke.hor.force'] = geckos_csv['fl.stroke.hor.force']*1000
    geckos_csv['bl.slap.force'] = geckos_csv['bl.slap.force'] * 1000
    geckos_csv['bl.stroke.ver.force'] =  geckos_csv['bl.stroke.ver.force']*1000
    geckos_csv['bl.stroke.hor.force'] = geckos_csv['bl.stroke.hor.force'] * 1000

    # Imput gecko weights
    geckos_csv['weight_g'] = geckos_csv['geckoID']    
    geckos_csv['weight_g'] = geckos_csv['weight_g'].map(
        {3: 5.4, 6: 4.89, 7: 5.71, 8: 6.9 , 9: 6.6, 10: 5.3}
        )
    
    # Transform weights in gram to mN and impulses per stride
    geckos_csv['weight_mN'] = geckos_csv['weight_g']/1000.0*9.8*1000
    geckos_csv['weight_imp'] = geckos_csv['weight_mN']*geckos_csv['period']/2
    
    # See excel sheet of equations for calculations
    # Finding the impulse for only a single stride, not an entire cycle
    # Add buoyancy of individual geckos
    geckos_csv['buoyancy_N'] = geckos_csv['geckoID']   
    geckos_csv['buoyancy_N'] = geckos_csv['buoyancy_N'].map(
        {3: 0.023462094, 6: 0.03894067, 7: 0.036983704, 8: 0.044893951,\
        9: 0.03387716, 10: 0.031308718}
        )
    geckos_csv['buoyancy_mN'] =  geckos_csv['buoyancy_N']*1000.0
    geckos_csv['buoyancy_imp'] = geckos_csv['buoyancy_mN']*geckos_csv['period']/2
    
    # Imput surface tension of each gecko
    geckos_csv['surface_tension_N'] = geckos_csv['geckoID']       
    tmp1 = geckos_csv[geckos_csv['expt']=='water']['surface_tension_N'].map(
        {3: 0.00316, 6: 0.00304, 7: 0.00330, 8: 0.00338,\
        9: 0.00308, 10: 0.00308}
        )
    tmp2 = geckos_csv[geckos_csv['expt']=='soap']['surface_tension_N'].map(
        {3: 0.00234, 6: 0.00259, 7: 0.00223, 8: 0.00258,\
        9: 0.00218, 10: 0.00218}
        )
    geckos_csv['surface_tension_N'] = pd.concat([tmp2,tmp1]).reset_index()['surface_tension_N']
    geckos_csv['surface_tension_mN'] =  geckos_csv['surface_tension_N']*1000.0
    geckos_csv['surface_tension_imp'] = geckos_csv['surface_tension_mN'] * geckos_csv['period']/2
    
    # Gecko body measurements
    geckos_csv['fl_mm'] = geckos_csv['geckoID']   
    geckos_csv['fl_mm'] = geckos_csv['fl_mm'].map(
        {3: 19.9, 6: 19.3, 7: 21, 8: 19,\
        9: 17.5, 10: 19.3}
        )    
    geckos_csv['bl_mm'] = geckos_csv['geckoID']   
    geckos_csv['bl_mm'] = geckos_csv['bl_mm'].map(
        {3: 24.1, 6: 22.8, 7: 23, 8: 19.2,\
        9: 26.9, 10: 24.3}
        )        
    geckos_csv['tail_mm'] = geckos_csv['geckoID']   
    geckos_csv['tail_mm'] = geckos_csv['tail_mm'].map(
        {3: 45.65, 6: 63.8, 7: 39.2, 8: 48.1,\
        9: 58.6, 10: 59.2}
        )    
    geckos_csv['svl_mm'] = geckos_csv['geckoID']   
    geckos_csv['svl_mm'] = geckos_csv['svl_mm'].map(
        {3: 59.91, 6: 54.4, 7: 59.6, 8: 57.4,\
        9: 58.6, 10: 56.5}
        )    
    
    # Calculate foot drag for each gecko
    geckos_csv['fl_drag'] = (999.97 * \
       (((1.004 * (10**(-6))) * (geckos_csv['fl_mm']/1000))**(1/2)) * \
       ((geckos_csv['avg_strokevel_hor']/1000)**(3/2))) + \
       (999.97 * ((geckos_csv['avg_strokevel_hor']/1000)**2) * (geckos_csv['fl_mm']/1000))

    geckos_csv['bl_drag'] = (999.97 * \
       (((1.004 * (10**(-6))) * (geckos_csv['bl_mm']/1000))**(1/2)) * \
       ((geckos_csv['avg_bstrokevel_hor']/1000)**(3/2))) + \
       (999.97 * ((geckos_csv['avg_bstrokevel_hor']/1000)**2)  * (geckos_csv['bl_mm']/1000))
       
       
    # find total vertical and horizongal forces generated per stride
    geckos_csv['vert_forces'] = geckos_csv['fl.slap.force']+                \
        geckos_csv['bl.slap.force']+geckos_csv['bl.stroke.ver.force']+      \
        geckos_csv['fl.stroke.ver.force']
    geckos_csv['hor_forces'] = geckos_csv['bl.stroke.hor.force']+           \
        geckos_csv['fl.stroke.hor.force'] + geckos_csv['total force']
    
    # percentage of force generated by a slap
    geckos_csv['slap_forces_per'] = geckos_csv['fl.slap.force']+            \
        geckos_csv['bl.slap.force'] / geckos_csv['vert_forces']
    
    # Total drag with feet, body, and tail
    geckos_csv['total_drag_w_feet'] = geckos_csv['total drag'] +            \
        geckos_csv['bl_drag'] + geckos_csv['fl_drag']
    
    
    # Impulse of drag
    geckos_csv['fl_drag_imp'] = geckos_csv['fl_drag']*geckos_csv['avg_fl_stroketime']/1000*1000
    geckos_csv['bl_drag_imp'] = geckos_csv['bl_drag']*geckos_csv['avg_bl_stroketime']/1000*1000
    geckos_csv['body_drag_imp'] = geckos_csv['body drag']*geckos_csv['period']/2
    geckos_csv['tail_drag_imp'] = geckos_csv['tail drag']*geckos_csv['period']/2
    geckos_csv['tot_drag_w_feet_imp'] = geckos_csv['fl_drag_imp'] + geckos_csv['bl_drag_imp'] + \
        geckos_csv['body_drag_imp'] + geckos_csv['tail_drag_imp']
        
    # Impulses converted to mN
    geckos_csv['fl.slap.imp'] = geckos_csv['fl.slap.imp']*1000
    geckos_csv['fl.stroke.ver.imp'] = geckos_csv['fl.stroke.ver.imp']*1000  
    geckos_csv['fl.stroke.hor.imp'] = geckos_csv['fl.stroke.hor.imp']*1000
    geckos_csv['bl.slap.imp'] = geckos_csv['bl.slap.imp']*1000
    geckos_csv['bl.stroke.ver.imp'] = geckos_csv['bl.stroke.ver.imp']*1000  
    geckos_csv['bl.stroke.hor.imp'] = geckos_csv['bl.stroke.hor.imp']*1000
    
    geckos_csv['body_imp'] = geckos_csv['body force']*geckos_csv['period']/2
    geckos_csv['tail_imp'] = geckos_csv['tail force']*geckos_csv['period']/2
    
    geckos_csv['total_vert_imp'] = geckos_csv['fl.slap.imp'] +geckos_csv['fl.stroke.ver.imp']+\
        geckos_csv['bl.slap.imp']+geckos_csv['bl.stroke.ver.imp'] + geckos_csv['buoyancy_imp'] + \
        geckos_csv['surface_tension_imp']
    geckos_csv['total_hor_imp'] = geckos_csv['fl.stroke.hor.imp']+\
        geckos_csv['bl.stroke.hor.imp']+geckos_csv['body_imp']+geckos_csv['tail_imp']
    
    
    # Find contributions
    geckos_csv['buoyancy/tot_vert_imp'] = geckos_csv['buoyancy_imp']/geckos_csv['total_vert_imp']*100
    geckos_csv['surf_tension/tot_vert_imp'] = geckos_csv['surface_tension_imp']/geckos_csv['total_vert_imp']*100
    geckos_csv['slap/tot_vert_imp'] = geckos_csv['fl.slap.imp']/geckos_csv['total_vert_imp']*100
    geckos_csv['stroke_vert/tot_vert_imp'] = (geckos_csv['fl.stroke.ver.imp']+geckos_csv['bl.stroke.ver.imp']) / geckos_csv['total_vert_imp']*100
    geckos_csv['stroke_hor/tot_hor_imp'] = (geckos_csv['fl.stroke.hor.imp']+geckos_csv['bl.stroke.hor.imp']) / geckos_csv['total_hor_imp']*100
    geckos_csv['undulation/tot_hor_imp'] = (geckos_csv['body_imp']+geckos_csv['tail_imp'])/geckos_csv['total_hor_imp']*100
    
    

    geckos_csv.to_csv("python_edited_geckos_all.csv", sep=',',index=False)
    
    
    # ------
    
    # Average the trials across individuals to find average per individual
    ind_gecko_avg = geckos_csv.groupby(['expt','geckoID']).mean().reset_index()
    ind_gecko_avg.to_csv("ind_gecko_avg.csv", sep=',',index=False)

    
    # Get overall average
    averages = ind_gecko_avg.groupby('expt').mean()
    std = ind_gecko_avg.groupby('expt').std()
    means = pd.concat([averages,std])
    
    means.to_csv("means.csv", sep=',',index=False)
    
    # Label low height geckos to high height geckos for drag forces only in water group
    avg_headht_water = ind_gecko_avg[ind_gecko_avg['expt']=='water'].mean()['head.ht']
    ind_gecko_avg['head_group'] = np.where(ind_gecko_avg['head.ht']<avg_headht_water, 'low', 'high')
    # Get average drag forces for only the water group
    ind_gecko_avg_water =  ind_gecko_avg[ind_gecko_avg['expt']=='water']
    drag_hilo = ind_gecko_avg_water.groupby('head_group').mean()['total drag']
    print(drag_hilo)
    drag_hi = ind_gecko_avg_water[ind_gecko_avg_water['head_group']=='high']
    drag_lo = ind_gecko_avg_water[ind_gecko_avg_water['head_group']=='low']
    T_val, p_val = stats.ttest_ind(drag_hi['total drag'],drag_lo['total drag'])
    print('T test of head high vs head low: T=', T_val, 'p=', p_val)

    
    
    # Rest of code is for graphing drag, thrust, drag/thurst ratio. Ignore
    dragthurst = ind_gecko_avg.groupby('expt').mean()['total force'].reset_index()
    dragthurst.columns=['expt','thrust']
    dragthurst['thurst_std'] = (ind_gecko_avg.groupby('expt').std().reset_index())['total force']
    dragthurst['drag'] = (ind_gecko_avg.groupby('expt').mean().reset_index())['total drag']
    dragthurst['drag_std'] = (ind_gecko_avg.groupby('expt').std().reset_index())['total drag']
    dragthurst['thurst/drag'] = dragthurst['thrust']/dragthurst['drag']
    
    fig = plt.figure()
    ax = fig.gca()
    plt.style.use('ggplot')
    # ax.grid(color='lightgray', linestyle='-', linewidth=1)
    # plt.ticklabel_format(useOffset=False) # turn off scientific plotting
    # fig.suptitle('Ge', fontsize=20)

    ax.bar([0,1],dragthurst['thrust'],yerr=dragthurst['thurst_std'])
    
    plt.xticks([0,1], dragthurst['expt'])
    plt.ylabel('Thurst (mN)')
    ax.set_facecolor('white')
    # ax.legend(loc='lower left').draggable()

    plt.subplots_adjust(bottom=None, top=0.9)
    fig.savefig('thurst.png')
    plt.show()
    
    
    fig = plt.figure()
    ax = fig.gca()
    plt.style.use('ggplot')
    # ax.grid(color='lightgray', linestyle='-', linewidth=1)
    # plt.ticklabel_format(useOffset=False) # turn off scientific plotting
    # fig.suptitle('Ge', fontsize=20)

    ax.bar([0,1],dragthurst['drag'],yerr=dragthurst['drag_std'])
    
    plt.xticks([0,1], dragthurst['expt'])
    plt.ylabel('Drag (mN)')
    ax.set_facecolor('white')
    # ax.legend(loc='lower left').draggable()

    plt.subplots_adjust(bottom=None, top=0.9)
    fig.savefig('drag.png')
    plt.show()
    
    
    fig = plt.figure()
    ax = fig.gca()
    plt.style.use('ggplot')
    # ax.grid(color='lightgray', linestyle='-', linewidth=1)
    # plt.ticklabel_format(useOffset=False) # turn off scientific plotting
    # fig.suptitle('Ge', fontsize=20)

    ax.bar([0,1],dragthurst['thurst/drag'])

    plt.xticks([0,1], dragthurst['expt'])
    plt.ylabel('Thurst/Drag Percentage')
    ax.set_facecolor('white')
    # ax.legend(loc='lower left').draggable()

    plt.subplots_adjust(bottom=None, top=0.9)
    fig.savefig('thurstdrag_ratio.png')
    plt.show()

    plt.close('all')