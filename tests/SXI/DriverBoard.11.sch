v 20100214 2
C 40000 40000 0 0 0 Noqsi-title-B.sym
{
T 50000 40500 5 10 1 1 0 0 1
date=$Date: Tue Nov 30 01:00:53 2010 -0700$
T 53900 40500 5 10 1 1 0 0 1
rev=$Revision$
T 55400 40200 5 10 1 1 0 0 1
auth=$Author$
T 50200 40800 5 8 1 1 0 0 1
fname=$Source$
T 53200 41200 5 14 1 1 0 4 1
title=Driver Board
}
C 41700 43000 1 0 0 MAX4508.sym
{
T 43400 46200 5 10 1 1 0 0 1
refdes=U4
}
C 41800 48200 1 0 0 resistor.sym
{
T 42100 48600 5 10 0 0 0 0 1
device=RESISTOR
T 42000 48500 5 10 1 1 0 0 1
refdes=R41
T 42100 48000 5 10 1 1 0 0 1
value=1k
T 41800 48200 5 10 0 1 0 0 1
footprint=1210
T 41800 48200 5 10 0 1 0 0 1
spec=5% 1/2W
}
C 41800 47200 1 0 0 DoubleCap.sym
{
T 42000 47700 5 10 1 1 0 0 1
refdes=C7
}
C 41700 47100 1 0 0 gnd-1.sym
C 44000 48200 1 0 1 resistor.sym
{
T 43700 48600 5 10 0 0 0 6 1
device=RESISTOR
T 43800 48500 5 10 1 1 0 6 1
refdes=R42
T 43700 48000 5 10 1 1 0 6 1
value=1k
T 44000 48200 5 10 0 1 0 6 1
footprint=1210
T 44000 48200 5 10 0 1 0 6 1
spec=5% 1/2W
}
C 44000 47200 1 0 1 DoubleCap.sym
{
T 43800 47700 5 10 1 1 0 6 1
refdes=C8
}
C 44100 47100 1 0 1 gnd-1.sym
N 43100 46400 43100 48300 4
N 42700 46400 42700 48300 4
U 44200 45200 44200 44100 10 -1
N 43600 44700 44000 44700 4
{
T 43500 44500 5 10 1 1 0 0 1
netname=HCOM
}
C 44000 44700 1 270 0 busripper-1.sym
N 44000 48300 44000 49000 4
{
T 44100 48900 5 10 1 1 0 0 1
netname=-15
}
C 44000 49000 1 90 0 busripper-1.sym
N 41800 48300 41800 49000 4
{
T 41900 48900 5 10 1 1 0 0 1
netname=+15
}
C 41800 49000 1 90 0 busripper-1.sym
C 41700 46100 1 0 0 gnd-1.sym
N 42300 46400 41800 46400 4
N 42200 43000 42200 42300 4
{
T 42000 42400 5 10 1 1 90 2 1
netname=HKA0
}
C 42200 42300 1 90 1 busripper-1.sym
N 42500 43000 42500 42300 4
{
T 42300 42400 5 10 1 1 90 2 1
netname=HKA1
}
C 42500 42300 1 90 1 busripper-1.sym
N 42800 43000 42800 42300 4
{
T 42600 42400 5 10 1 1 90 2 1
netname=HKA2
}
C 42800 42300 1 90 1 busripper-1.sym
N 43100 43000 43100 42300 4
{
T 42900 42400 5 10 1 1 90 2 1
netname=HEN2
}
C 43100 42300 1 90 1 busripper-1.sym
T 44100 43900 9 10 1 0 0 0 1
HK
N 41700 45700 40800 45700 4
{
T 40800 45700 5 10 1 1 0 0 1
netname=RGHI
}
C 40800 45700 1 90 0 busripper-1.sym
N 41700 45400 40800 45400 4
{
T 40800 45400 5 10 1 1 0 0 1
netname=RGLO
}
C 40800 45400 1 90 0 busripper-1.sym
N 41700 45100 40800 45100 4
{
T 40800 45100 5 10 1 1 0 0 1
netname=SGHI
}
C 40800 45100 1 90 0 busripper-1.sym
N 41700 44800 40800 44800 4
{
T 40800 44800 5 10 1 1 0 0 1
netname=SGLO
}
C 40800 44800 1 90 0 busripper-1.sym
N 41700 44500 40800 44500 4
{
T 40800 44500 5 10 1 1 0 0 1
netname=TGHI
}
C 40800 44500 1 90 0 busripper-1.sym
N 41700 44200 40800 44200 4
{
T 40800 44200 5 10 1 1 0 0 1
netname=TGLO
}
C 40800 44200 1 90 0 busripper-1.sym
U 41800 42100 43100 42100 10 0
U 40600 46100 40600 43500 10 0
U 41400 49200 44000 49200 10 0
T 40800 49100 9 10 1 0 0 0 1
Power
T 40500 46200 9 10 1 0 0 0 1
HK
T 41100 42100 9 10 1 0 0 0 1
Control
C 47200 43000 1 0 0 MAX4508.sym
{
T 48900 46200 5 10 1 1 0 0 1
refdes=U5
}
C 47300 48200 1 0 0 resistor.sym
{
T 47600 48600 5 10 0 0 0 0 1
device=RESISTOR
T 47500 48500 5 10 1 1 0 0 1
refdes=R43
T 47600 48000 5 10 1 1 0 0 1
value=1k
T 47300 48200 5 10 0 1 0 0 1
footprint=1210
T 47300 48200 5 10 0 1 0 0 1
spec=5% 1/2W
}
C 47300 47200 1 0 0 DoubleCap.sym
{
T 47500 47700 5 10 1 1 0 0 1
refdes=C9
}
C 47200 47100 1 0 0 gnd-1.sym
C 49500 48200 1 0 1 resistor.sym
{
T 49200 48600 5 10 0 0 0 6 1
device=RESISTOR
T 49300 48500 5 10 1 1 0 6 1
refdes=R44
T 49200 48000 5 10 1 1 0 6 1
value=1k
T 49500 48200 5 10 0 1 0 6 1
footprint=1210
T 49500 48200 5 10 0 1 0 6 1
spec=5% 1/2W
}
C 49500 47200 1 0 1 DoubleCap.sym
{
T 49300 47700 5 10 1 1 0 6 1
refdes=C10
}
C 49600 47100 1 0 1 gnd-1.sym
N 48600 46400 48600 48300 4
N 48200 46400 48200 48300 4
U 49700 45200 49700 44100 10 -1
N 49100 44700 49500 44700 4
{
T 49000 44500 5 10 1 1 0 0 1
netname=HCOM
}
C 49500 44700 1 270 0 busripper-1.sym
N 49500 48300 49500 49000 4
{
T 49600 48900 5 10 1 1 0 0 1
netname=-15
}
C 49500 49000 1 90 0 busripper-1.sym
N 47300 48300 47300 49000 4
{
T 47400 48900 5 10 1 1 0 0 1
netname=+15
}
C 47300 49000 1 90 0 busripper-1.sym
C 47200 46100 1 0 0 gnd-1.sym
N 47800 46400 47300 46400 4
N 47700 43000 47700 42300 4
{
T 47500 42400 5 10 1 1 90 2 1
netname=HKA0
}
C 47700 42300 1 90 1 busripper-1.sym
N 48000 43000 48000 42300 4
{
T 47800 42400 5 10 1 1 90 2 1
netname=HKA1
}
C 48000 42300 1 90 1 busripper-1.sym
N 48300 43000 48300 42300 4
{
T 48100 42400 5 10 1 1 90 2 1
netname=HKA2
}
C 48300 42300 1 90 1 busripper-1.sym
N 48600 43000 48600 42300 4
{
T 48400 42400 5 10 1 1 90 2 1
netname=HEN3
}
C 48600 42300 1 90 1 busripper-1.sym
N 47200 45700 45500 45700 4
{
T 45500 45700 5 10 1 1 0 0 1
netname=PHHI
}
C 45500 45700 1 90 0 busripper-1.sym
N 47200 45400 45500 45400 4
{
T 45500 45400 5 10 1 1 0 0 1
netname=PHLO
}
C 45500 45400 1 90 0 busripper-1.sym
N 47200 45100 45500 45100 4
{
T 45500 45100 5 10 1 1 0 0 1
netname=VIHI
}
C 45500 45100 1 90 0 busripper-1.sym
N 47200 44800 45500 44800 4
{
T 45500 44800 5 10 1 1 0 0 1
netname=VILO
}
C 45500 44800 1 90 0 busripper-1.sym
N 47200 44500 45500 44500 4
{
T 45500 44500 5 10 1 1 0 0 1
netname=VSHI
}
C 45500 44500 1 90 0 busripper-1.sym
N 47200 44200 45500 44200 4
{
T 45500 44200 5 10 1 1 0 0 1
netname=VSLO
}
C 45500 44200 1 90 0 busripper-1.sym
U 47300 42100 48600 42100 10 0
U 45300 46100 45300 43500 10 0
U 44000 49200 49700 49200 10 1
T 49600 43900 9 10 1 0 0 0 1
HK
T 45200 46200 9 10 1 0 0 0 1
HK
T 46600 42100 9 10 1 0 0 0 1
Control
T 50500 40200 9 10 1 0 0 0 1
11
T 43500 40700 5 25 1 1 0 4 1
title=Clock Level Housekeeping
N 41700 43900 40800 43900 4
{
T 40800 43900 5 10 1 1 0 0 1
netname=IG1VHI
}
C 40800 43900 1 90 0 busripper-1.sym
N 41700 43600 40800 43600 4
{
T 40800 43600 5 10 1 1 0 0 1
netname=IG1VLO
}
C 40800 43600 1 90 0 busripper-1.sym
C 47100 43300 1 0 0 gnd-1.sym
N 47200 43900 46500 43900 4
N 46500 43900 46500 49000 4
{
T 45800 48800 5 10 1 1 0 0 1
netname=+2.5ref
}
C 46500 49000 1 90 0 busripper-1.sym
T 52000 40200 9 10 1 0 0 0 1
19
