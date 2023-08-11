from gurobipy import *
import pandas as pd
import numpy as np
import datetime as dt
import plotly.express as px
import plotly.figure_factory as ff

# Read excel sheets
df1 = pd.read_excel('OR110-1_case01.xlsx','Instance 1')
df2 = pd.read_excel('OR110-1_case01.xlsx','Instance 1', header = 1)
df1 = df1.fillna(0)
df2 = df2.fillna(0)

# j jobs, getting j
jobs = range(2 * (len(df1['Job ID'])-1))
jobs_cnt = len(df1['Job ID'])-1

# i processes, setting i to 2
processes = range(2)

# k machines, setting k to 5
machines = range(5)

# getting splitting timing
splitting_timing = df1['Splitting Timing'].values
splitting_timing = np.delete(splitting_timing, 0)

# setting start time to 7.5 (start at 7:30 AM)
start_time = 7.5

# getting processing time, in hours
processing_time = []
for i in processes:
    processing_time.append(list(df2['Process ' + str(i+1) + '.1']))

# getting due time (number of hours after start time)
due_time = df1['Due Time'].values
due_time = np.delete(due_time, 0)

# seperating each job into two jobs by the spliting timing, getting a new processing time and a new due time
p = []
for j in range(0, jobs_cnt):
    p.append(0.0)
    for i in processes:
        if i < int(splitting_timing[j]):
            p[j] += processing_time[i][j]

for j in range(0, jobs_cnt):
    p.append(0.0)
    for i in processes:
        if i >= int(splitting_timing[j]):
            p[j + jobs_cnt] += processing_time[i][j]

d = []
for j in range(0, jobs_cnt):
    hour = int(due_time[j].strftime('%H'))
    minute = int(due_time[j].strftime('%M'))
    d.append(hour)
    d[j] += float(minute) / 60
    d[j] -= start_time

for j in range(0, jobs_cnt):
    hour = int(due_time[j].strftime('%H'))
    minute = int(due_time[j].strftime('%M'))
    d.append(hour)
    d[j + jobs_cnt] += float(minute) / 60
    d[j + jobs_cnt] -= start_time

# Set big number for mathematic model use
M = quicksum(p[j] for j in jobs)

# building a mathemathic model
case1 = Model("case1")    
    
# adding variables
x = [] # completion time of job j
for j in jobs:
    x.append(case1.addVar(lb = 0, vtype = GRB.CONTINUOUS, name = "x" + str(j+1)))
    
t = [] # equals to 1 if job j is tardy or 0 otherwise 
for j in jobs:
    t.append(case1.addVar(lb = 0, vtype = GRB.BINARY, name = "t" + str(j+1)))
    
y = [] # equals to 1 if job j is scheduled on machine k or 0 otherwise 
for k in machines:
    y.append([])
    for j in jobs:
        y[k].append(case1.addVar(lb = 0, vtype = GRB.BINARY, name = "y" + str(k+1) + "," + str(j+1)))
        
z = [] # equals to 1 if job j is scheduled before job i or 0 otherwise, i < j 
for i in jobs:
    z.append([])
    for j in jobs:
        z[i].append(case1.addVar(lb = 0, vtype = GRB.BINARY, name = "z" + str(i+1) + "," + str(j+1)))
        
# setting the objective function1 (objective value is the number of tardy jobs)
case1.setObjective(quicksum(t[j] for j in jobs if j >= jobs_cnt) , GRB.MINIMIZE) 

# adding constraints
case1.addConstrs(x[j] - d[j] <= M * t[j] for j in jobs if j >= jobs_cnt)

for i in jobs:
    for j in jobs:
            if i < j:
                case1.addConstrs(x[j] + p[i] - x[i] <= M * ((1 - z[i][j]) + (2 - y[k][i] - y[k][j])) for k in machines)
                
for i in jobs:
    for j in jobs:
            if i < j:
                case1.addConstrs(x[i] + p[j] - x[j] <= M * (z[i][j] + (2 - y[k][i] - y[k][j])) for k in machines)
                
case1.addConstrs(x[j] >= p[j] for j in jobs)
                
case1.addConstrs(quicksum(y[k][j] for k in machines) == 1 for j in jobs)

case1.addConstrs(y[0][j] == 0 for j in jobs)

# add constraints
for i in range(0, jobs_cnt):
    j = i + jobs_cnt
    case1.addConstrs(x[j] + p[i] - x[i] <= M * (1 - z[i][j]) for k in machines)
                
for i in range(0, jobs_cnt):
    j = i + jobs_cnt
    case1.addConstrs(x[i] + p[j] - x[j] <= M * z[i][j] for k in machines)

for i in range(0, jobs_cnt):
    case1.addConstr(z[i][i + jobs_cnt] == 0)
    
# solving (first-phase)
case1.optimize()

# getting objective value1 (the number of tardy jobs) 
tardy_cnt = case1.objVal

# adding variables
w = case1.addVar(lb = 0, vtype = GRB.CONTINUOUS, name = "w") # the maximum of xj

# setting the objective function2 (objective value is the makespan) 
case1.setObjective(w , GRB.MINIMIZE) 

# adding constraints for number of tardy jobs (according to the answer of objective value1)
case1.addConstr(quicksum(t[j] for j in jobs if j >= jobs_cnt) == tardy_cnt)
case1.addConstrs(w >= x[j] for j in jobs)

# solving (second-phase)
case1.optimize()

# getting objective value2 (the makespan) 
print("z* =", round(q2.objVal,2))   

# setting
date = '2022-04-09 '
noon = '12:30'
evening = "17:30"
time_format = '%Y-%m-%d %H:%M:%S'

# getting start time and end time of each job from decision variables
start_time = []
for j in jobs:
    time = 7.5 + round(x[j].x,2) - round(p[j],2)
    miniute = int(time * 10 % 10 / 10 * 60)
    hour = int(time * 10 / 10)
    if(hour < 10):
        if(miniute < 10):
            start_time.append('0' + str(hour) + ':0' + str(miniute) + ':00')
        else:
            start_time.append('0' + str(hour) + ':' + str(miniute) + ':00')
    else:
        if(miniute < 10):
            start_time.append(str(hour) + ':0' + str(miniute) + ':00')
        else:
            start_time.append(str(hour) + ':' + str(miniute) + ':00')

end_time = []
for j in jobs:
    time = 7.5 + round(x[j].x,2)
    miniute = int(time * 10 % 10 / 10 * 60)
    hour = int(time * 10 / 10)
    if(hour < 10):
        if(miniute < 10):
            end_time.append('0' + str(hour) + ':0' + str(miniute) + ':00')
        else:
            end_time.append('0' + str(hour) + ':' + str(miniute) + ':00')
    else:
        if(miniute < 10):
            end_time.append(str(hour) + ':0' + str(miniute) + ':00')
        else:
            end_time.append(str(hour) + ':' + str(miniute) + ':00')

job = []
for i in jobs:
    job.append((date + start_time[i], date + end_time[i]))

# getting machine for each job from decision variables
machine = []
for j in jobs:
    for k in machines:
        if y[k][j].x == 1:
            machine_id = k + 1
    machine.append(str(machine_id))

# scheuling
blank = (date+'07:30:00',date+'07:30:00')
scheuling = [dict(Machine="1", Start=blank[0],Finish=blank[1], Order="1, "+noon),
             dict(Machine="2", Start=blank[0],Finish=blank[1], Order="2, "+noon),
             dict(Machine="3", Start=blank[0],Finish=blank[1], Order="3, "+noon),
             dict(Machine="4", Start=blank[0],Finish=blank[1], Order="4, "+noon),
             dict(Machine="5", Start=blank[0],Finish=blank[1], Order="5, "+noon)]

for j in jobs:
    i = j
    if j >= jobs_cnt:
        i = j - jobs_cnt
    if d[j] == 5:
        scheuling.append(dict(Machine = machine[j], Start=job[j][0], Finish=job[j][1], Order= str(i+1) + ", " + noon))
    else:
        scheuling.append(dict(Machine = machine[j], Start=job[j][0], Finish=job[j][1], Order= str(i+1) + ", " + evening))

df = pd.DataFrame(scheuling)

# setting job name to the middle point (in figure)
def middle(job):
    return dt.datetime.strftime((dt.datetime.strptime(job[1],time_format)
             -dt.datetime.strptime(job[0],time_format))/2
                +dt.datetime.strptime(job[0],time_format),time_format)

mid_job = []
for j in jobs:
    mid_job.append(middle(job[j]))
    
annots = []
for j in jobs:
    i = j
    if j >= jobs_cnt:
        i = j - jobs_cnt
    annots.append(dict(x=mid_job[j], y=(int(machine[j])-1), text=str(i+1), showarrow=False, font=dict(color='white')))
    
# figure
fig = px.timeline(df, x_start="Start", x_end="Finish",
                  y="Machine", color="Order",title='Instance 1')
fig.update_yaxes(autorange="reversed")
fig.update_layout(
    font_family = 'Arial',
    font_size = 16,
    width = 800,
    height = 500,
)
fig['layout']['annotations'] = annots
fig.show()