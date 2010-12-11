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
C 43900 42200 1 0 0 LVDS31.sym
{
T 45100 45700 5 10 1 1 0 6 1
refdes=U14
}
C 51400 42200 1 0 0 LVDS31.sym
{
T 52600 45700 5 10 1 1 0 6 1
refdes=U15
}
T 40500 40400 9 25 1 0 0 0 1
LVDS Drivers
C 43900 43600 1 0 0 gnd-1.sym
C 43900 42400 1 0 0 gnd-1.sym
N 42500 45100 44000 45100 4
{
T 42600 45100 5 10 1 1 0 0 1
netname=\_VSTROBE\_
}
U 42300 46100 42300 41900 10 -1
C 42500 44700 1 90 1 busripper-1.sym
N 42500 44700 44000 44700 4
{
T 42600 44700 5 10 1 1 0 0 1
netname=DVCD
}
C 42500 45100 1 90 1 busripper-1.sym
N 42500 43500 44000 43500 4
{
T 42600 43500 5 10 1 1 0 0 1
netname=\_DVCS\_
}
C 42500 43500 1 90 1 busripper-1.sym
N 47300 45500 46000 45500 4
{
T 47200 45500 5 10 1 1 0 6 1
netname=VSTROBE+
}
C 47300 45500 1 270 1 busripper-1.sym
N 47300 45100 46000 45100 4
{
T 47200 45100 5 10 1 1 0 6 1
netname=VSTROBE-
}
C 47300 45100 1 270 1 busripper-1.sym
N 47300 44700 46000 44700 4
{
T 47200 44700 5 10 1 1 0 6 1
netname=VCMD+
}
C 47300 44700 1 270 1 busripper-1.sym
N 47300 44300 46000 44300 4
{
T 47200 44300 5 10 1 1 0 6 1
netname=VCMD-
}
C 47300 44300 1 270 1 busripper-1.sym
N 47300 43900 46000 43900 4
{
T 47200 43900 5 10 1 1 0 6 1
netname=DVCS+
}
C 47300 43900 1 270 1 busripper-1.sym
N 47300 43500 46000 43500 4
{
T 47200 43500 5 10 1 1 0 6 1
netname=DVCS-
}
C 47300 43500 1 270 1 busripper-1.sym
N 44000 44300 43800 44300 4
N 43800 44300 43800 47500 4
N 43800 45500 44000 45500 4
C 51400 43600 1 0 0 gnd-1.sym
C 51400 42400 1 0 0 gnd-1.sym
N 50000 45100 51500 45100 4
{
T 50100 45100 5 10 1 1 0 0 1
netname=TCEDAT
}
U 49800 46100 49800 41900 10 -1
C 50000 44700 1 90 1 busripper-1.sym
N 50000 44700 51500 44700 4
{
T 50100 44700 5 10 1 1 0 0 1
netname=HKDAT
}
C 50000 45100 1 90 1 busripper-1.sym
N 51500 43500 51500 42700 4
N 51500 44300 51300 44300 4
N 51300 44300 51300 47500 4
N 51300 45500 51500 45500 4
N 54800 45500 53500 45500 4
{
T 54700 45500 5 10 1 1 0 6 1
netname=TCEDAT-
}
C 54800 45500 1 270 1 busripper-1.sym
N 54800 45100 53500 45100 4
{
T 54700 45100 5 10 1 1 0 6 1
netname=TCEDAT+
}
C 54800 45100 1 270 1 busripper-1.sym
N 54800 44700 53500 44700 4
{
T 54700 44700 5 10 1 1 0 6 1
netname=HKDAT-
}
C 54800 44700 1 270 1 busripper-1.sym
N 54800 44300 53500 44300 4
{
T 54700 44300 5 10 1 1 0 6 1
netname=HKDAT+
}
C 54800 44300 1 270 1 busripper-1.sym
C 43800 47400 1 0 1 resistor.sym
{
T 43500 47800 5 10 0 0 0 6 1
device=RESISTOR
T 43600 47700 5 10 1 1 0 6 1
refdes=R48
T 43500 47200 5 10 1 1 0 6 1
value=22
T 43800 47400 5 10 0 1 0 6 1
footprint=2512
T 43800 47400 5 10 0 1 0 6 1
spec=5% 1W
}
C 43800 46700 1 0 1 resistor.sym
{
T 43500 47100 5 10 0 0 0 6 1
device=RESISTOR
T 43600 47000 5 10 1 1 0 6 1
refdes=R49
T 43500 46500 5 10 1 1 0 6 1
value=22
T 43800 46700 5 10 0 1 0 6 1
footprint=2512
T 43800 46700 5 10 0 1 0 6 1
spec=5% 1W
}
C 44900 47300 1 0 1 DoubleCap.sym
{
T 45000 47600 5 10 1 1 0 6 1
refdes=C13
}
C 45000 47200 1 0 1 gnd-1.sym
N 44000 47500 43800 47500 4
C 51300 47400 1 0 1 resistor.sym
{
T 51000 47800 5 10 0 0 0 6 1
device=RESISTOR
T 51100 47700 5 10 1 1 0 6 1
refdes=R50
T 51000 47200 5 10 1 1 0 6 1
value=22
T 51300 47400 5 10 0 1 0 6 1
footprint=2512
T 51300 47400 5 10 0 1 0 6 1
spec=5% 1W
}
C 51300 46700 1 0 1 resistor.sym
{
T 51000 47100 5 10 0 0 0 6 1
device=RESISTOR
T 51100 47000 5 10 1 1 0 6 1
refdes=R51
T 51000 46500 5 10 1 1 0 6 1
value=22
T 51300 46700 5 10 0 1 0 6 1
footprint=2512
T 51300 46700 5 10 0 1 0 6 1
spec=5% 1W
}
C 52400 47300 1 0 1 DoubleCap.sym
{
T 52500 47600 5 10 1 1 0 6 1
refdes=C14
}
C 52500 47200 1 0 1 gnd-1.sym
N 51500 47500 51300 47500 4
U 42200 48200 50800 48200 10 -1
N 50400 46800 50400 48000 4
{
T 49900 48000 5 10 1 1 0 0 1
netname=+3.3
}
C 50400 48000 1 90 0 busripper-1.sym
N 42900 46800 42900 48000 4
{
T 42400 48000 5 10 1 1 0 0 1
netname=+3.3
}
C 42900 48000 1 90 0 busripper-1.sym
T 41600 48200 9 10 1 0 0 0 1
Power
C 47200 46700 1 0 0 Connector.sym
{
T 47500 47000 5 10 1 1 0 4 1
refdes=J5
T 47200 46700 5 10 0 0 0 0 1
footprint=MDM25S
}
U 47500 42600 47500 46700 10 0
U 55000 44300 55000 46700 10 0
C 54700 46700 1 0 0 Connector.sym
{
T 55000 47000 5 10 1 1 0 4 1
refdes=J4
T 55300 46700 5 10 0 0 0 0 1
footprint=51pin
}
T 50300 40200 9 10 1 0 0 0 1
17
U 49800 41900 41600 41900 10 0
T 40900 41800 9 10 1 0 0 0 1
Control
N 42500 43100 44000 43100 4
{
T 42600 43100 5 10 1 1 0 0 1
netname=PCLK
}
C 42500 43100 1 90 1 busripper-1.sym
N 47300 43100 46000 43100 4
{
T 47200 43100 5 10 1 1 0 6 1
netname=VPCLK+
}
C 47300 43100 1 270 1 busripper-1.sym
N 47300 42700 46000 42700 4
{
T 47200 42700 5 10 1 1 0 6 1
netname=VPCLK-
}
C 47300 42700 1 270 1 busripper-1.sym
T 52000 40200 9 10 1 0 0 0 1
19