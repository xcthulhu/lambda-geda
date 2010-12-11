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
C 46500 43700 1 0 0 hkadc.sym
{
T 50300 47300 5 10 1 1 0 3 1
refdes=X26
}
N 49800 47500 49800 48200 4
{
T 49900 48100 5 10 1 1 0 0 1
netname=-15
}
C 49800 48200 1 90 0 busripper-1.sym
N 47200 47500 47200 48200 4
{
T 47300 48100 5 10 1 1 0 0 1
netname=+15
}
C 47200 48200 1 90 0 busripper-1.sym
U 46800 48400 49900 48400 10 0
T 46200 48300 9 10 1 0 0 0 1
Power
N 47800 47500 47800 48200 4
{
T 47900 47700 5 10 1 1 0 0 1
netname=+5ADC
}
C 47800 48200 1 90 0 busripper-1.sym
N 48500 47500 48500 48200 4
{
T 48600 48100 5 10 1 1 0 0 1
netname=+3.3
}
C 48500 48200 1 90 0 busripper-1.sym
C 48600 43400 1 0 1 gnd-1.sym
U 45900 47300 45900 46200 10 -1
N 46500 46800 46100 46800 4
{
T 46600 46600 5 10 1 1 0 6 1
netname=HCOM
}
C 46100 46800 1 90 1 busripper-1.sym
T 46000 46000 9 10 1 0 0 6 1
HK
T 50400 40200 9 10 1 0 0 0 1
12
U 52500 47300 52500 44000 10 -1
N 50500 46800 52300 46800 4
{
T 51700 46600 5 10 1 1 0 0 1
netname=\_HKCS\_
}
C 52300 46800 1 270 0 busripper-1.sym
T 52200 43800 9 10 1 0 0 0 1
Control
N 50500 46000 52300 46000 4
{
T 51700 45800 5 10 1 1 0 0 1
netname=PCLK
}
C 52300 46000 1 270 0 busripper-1.sym
N 50500 45600 52300 45600 4
{
T 51700 45400 5 10 1 1 0 0 1
netname=HKSD
}
C 52300 45600 1 270 0 busripper-1.sym
T 43300 40800 5 25 1 1 0 4 1
title=Housekeeping ADC
T 52000 40200 9 10 1 0 0 0 1
19