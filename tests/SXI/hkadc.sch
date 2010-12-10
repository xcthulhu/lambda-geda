v 20100214 2
C 40000 40000 0 0 0 Noqsi-title-B.sym
C 55100 44800 1 0 0 out-1.sym
{
T 55100 45100 5 10 0 0 0 0 1
device=OUTPUT
T 55100 45100 5 10 1 1 0 0 1
refdes=DOUT
}
N 53400 46600 53400 47500 4
C 50900 46200 1 0 0 gnd-1.sym
N 49900 44700 48600 44700 4
C 55700 44000 1 0 1 in-1.sym
{
T 55700 44300 5 10 0 0 0 6 1
device=INPUT
T 55700 44300 5 10 1 1 0 6 1
refdes=SCLK
}
C 55700 43600 1 0 1 in-1.sym
{
T 55700 43900 5 10 0 0 0 6 1
device=INPUT
T 55500 43400 5 10 1 1 0 6 1
refdes=\_CS\_
}
N 55100 44900 53600 44900 4
N 55100 44100 53600 44100 4
N 55100 43700 53600 43700 4
N 52100 45600 52100 47200 4
C 51100 42000 1 0 0 in-1.sym
{
T 51100 42300 5 10 0 0 0 0 1
device=INPUT
T 51100 42300 5 10 1 1 0 0 1
refdes=GND
}
C 51600 41800 1 0 0 gnd-1.sym
T 50200 41200 9 20 1 0 0 1 1
Housekeeping ADC
T 50500 40200 9 10 1 0 0 0 1
1
T 52100 40200 9 10 1 0 0 0 1
1
T 50200 40800 5 8 1 1 0 0 1
fname=$Source$
T 50000 40500 5 10 1 1 0 0 1
date=$Date: Tue Nov 30 01:00:53 2010 -0700$
T 53900 40500 5 10 1 1 0 0 1
rev=$Revision$
T 55400 40200 5 10 1 1 0 0 1
auth=$Author$
C 47700 44600 1 0 0 resistor.sym
{
T 48000 45000 5 10 0 0 0 0 1
device=RESISTOR
T 47900 44900 5 10 1 1 0 0 1
refdes=R1
T 48600 45500 5 10 0 0 0 0 1
footprint=0603
T 48100 44400 5 10 1 1 0 0 1
value=100k
T 48000 44200 5 10 1 1 0 0 1
spec=0.1%
}
N 47700 44700 47700 45500 4
N 47700 45500 46600 45500 4
N 46600 45500 46600 44900 4
N 46600 44900 46700 44900 4
C 48600 43700 1 0 0 resistor.sym
{
T 48900 44100 5 10 0 0 0 0 1
device=RESISTOR
T 48800 44000 5 10 1 1 0 0 1
refdes=R2
T 49500 44600 5 10 0 0 0 0 1
footprint=0603
T 48600 43500 5 10 1 1 0 0 1
value=63.4k
T 48900 43300 5 10 1 1 0 0 1
spec=0.1%
}
N 48600 43800 48600 46000 4
C 49400 43500 1 0 0 gnd-1.sym
C 45600 44400 1 0 0 in-1.sym
{
T 45600 44700 5 10 0 0 0 0 1
device=INPUT
T 45600 44700 5 10 1 1 0 0 1
refdes=AIN
}
N 46700 44500 46200 44500 4
C 52500 47400 1 0 0 resistor.sym
{
T 52800 47800 5 10 0 0 0 0 1
device=RESISTOR
T 52700 47700 5 10 1 1 0 0 1
refdes=R4
T 53400 48300 5 10 0 0 0 0 1
footprint=1210
T 52900 47200 5 10 1 1 0 0 1
value=100
}
C 51600 49000 1 0 0 in-1.sym
{
T 51600 49300 5 10 0 0 0 0 1
device=INPUT
T 51600 49300 5 10 1 1 0 0 1
refdes=+3.3
}
N 52500 47500 52500 49100 4
N 52500 49100 52200 49100 4
C 42400 40900 1 0 0 resistor.sym
{
T 42700 41300 5 10 0 0 0 0 1
device=RESISTOR
T 42600 41200 5 10 1 1 0 0 1
refdes=R6
T 42700 40700 5 10 1 1 0 0 1
value=1k
T 42400 40900 5 10 0 1 0 0 1
footprint=1210
T 42400 40900 5 10 0 1 0 0 1
spec=5% 1/2W
}
C 43300 40800 1 0 0 DoubleCap.sym
{
T 43500 41300 5 10 1 1 0 0 1
refdes=C2
}
C 44100 40700 1 0 0 gnd-1.sym
C 41800 40900 1 0 0 in-1.sym
{
T 41800 41200 5 10 0 0 0 0 1
device=INPUT
T 41800 41200 5 10 1 1 0 0 1
refdes=-15
}
C 42400 42500 1 0 0 resistor.sym
{
T 42700 42900 5 10 0 0 0 0 1
device=RESISTOR
T 42600 42800 5 10 1 1 0 0 1
refdes=R5
T 42700 42300 5 10 1 1 0 0 1
value=1k
T 42400 42500 5 10 0 1 0 0 1
footprint=1210
T 42400 42500 5 10 0 1 0 0 1
spec=5% 1/2W
}
C 43300 42400 1 0 0 DoubleCap.sym
{
T 43500 42900 5 10 1 1 0 0 1
refdes=C1
}
C 44100 42300 1 0 0 gnd-1.sym
C 41800 42500 1 0 0 in-1.sym
{
T 41800 42800 5 10 0 0 0 0 1
device=INPUT
T 41800 42800 5 10 1 1 0 0 1
refdes=+15
}
C 43100 41500 1 0 0 LT1078S8-pwr.sym
{
T 43800 42300 5 10 0 0 0 0 1
device=LT1078IS8
T 43600 41900 5 10 1 1 0 0 1
refdes=U1
T 43800 42900 5 10 0 0 0 0 1
symversion=0.1
T 44100 42100 5 10 0 0 0 0 1
footprint=SO8
}
N 43300 41500 43300 41000 4
N 43300 42300 43300 42600 4
C 46700 45100 1 180 1 LT1078S8.sym
{
T 47400 44300 5 10 0 0 180 6 1
device=LT1078IS8
T 47400 44500 5 10 1 1 180 6 1
refdes=U1
T 47400 43700 5 10 0 0 180 6 1
symversion=0.1
T 47700 44500 5 10 0 0 180 6 1
footprint=SO8
}
C 49900 45300 1 180 1 LT1078S8.sym
{
T 50600 44500 5 10 0 0 180 6 1
device=LT1078IS8
T 50600 44700 5 10 1 1 180 6 1
refdes=U1
T 50600 43900 5 10 0 0 180 6 1
symversion=0.1
T 50900 44700 5 10 0 0 180 6 1
footprint=SO8
T 52200 43100 5 10 0 1 0 0 1
slot=2
}
N 49900 45100 49900 45600 4
N 49900 45600 50900 45600 4
N 50900 45600 50900 44900 4
C 40700 46000 1 0 0 hkadc.sym
{
T 44400 49600 5 10 1 1 0 3 1
refdes=X?
T 43300 46500 5 10 0 1 0 0 1
graphical=1
}
C 51100 42800 1 0 0 128S102.sym
{
T 52400 44200 5 10 1 1 0 3 1
refdes=U2
}
C 52700 42500 1 0 0 gnd-1.sym
C 51900 42500 1 0 0 gnd-1.sym
N 51100 44700 51100 42800 4
N 51100 42800 52000 42800 4
C 53700 44200 1 0 0 gnd-1.sym
N 50900 44900 51100 44900 4
C 48600 45900 1 0 0 resistor.sym
{
T 48900 46300 5 10 0 0 0 0 1
device=RESISTOR
T 48800 46200 5 10 1 1 0 0 1
refdes=R3
T 49500 46800 5 10 0 0 0 0 1
footprint=0603
T 48700 45700 5 10 1 1 0 0 1
value=39.2k
T 48900 45500 5 10 1 1 0 0 1
spec=0.1%
}
N 49500 46000 52100 46000 4
C 51000 46300 1 0 0 capacitor-1.sym
{
T 51200 47000 5 10 0 0 0 0 1
device=CAPACITOR
T 51200 46800 5 10 1 1 0 0 1
refdes=C3
T 51200 47200 5 10 0 0 0 0 1
symversion=0.1
T 51600 46300 5 10 1 1 0 0 1
value=1uF
T 51000 46300 5 10 0 1 0 0 1
footprint=1210
T 51000 46300 5 10 0 1 0 0 1
spec=50WVDC X7R
}
C 51000 47100 1 0 0 in-1.sym
{
T 51000 47400 5 10 0 0 0 0 1
device=INPUT
T 51000 47400 5 10 1 1 0 0 1
refdes=+5
}
N 52100 47200 51600 47200 4
N 51900 46500 52100 46500 4
C 53000 45800 1 0 0 SMBT2222-1.sym
{
T 53500 46250 5 10 1 1 0 0 1
refdes=Q1
}
C 52100 46100 1 0 0 resistor.sym
{
T 52400 46500 5 10 0 0 0 0 1
device=RESISTOR
T 52300 46400 5 10 1 1 0 0 1
refdes=R7
T 52500 45900 5 10 1 1 0 0 1
value=10k
T 52100 46100 5 10 0 1 0 0 1
footprint=0603
T 52100 46100 5 10 0 1 0 0 1
spec=5% 1/10W
}
C 53400 45400 1 0 0 DoubleCap.sym
{
T 53600 45900 5 10 1 1 0 0 1
refdes=C4
}
C 54200 45300 1 0 0 gnd-1.sym
N 53400 45600 53400 45800 4
N 53400 45600 52700 45600 4
N 53800 44500 53600 44500 4
