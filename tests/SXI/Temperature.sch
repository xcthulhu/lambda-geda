v 20100214 2
C 40000 40000 0 0 0 Noqsi-title-B.sym
C 42800 46200 1 0 0 in-1.sym
{
T 42800 46500 5 10 0 0 0 0 1
device=INPUT
T 42800 46500 5 10 1 1 0 0 1
refdes=I+
}
C 51500 44600 1 0 0 out-1.sym
{
T 51500 44900 5 10 0 0 0 0 1
device=OUTPUT
T 51500 44900 5 10 1 1 0 0 1
refdes=DOUT
}
N 43400 44700 47600 44700 4
N 43400 46300 43400 49200 4
C 43400 49100 1 0 0 resistor.sym
{
T 43700 49500 5 10 0 0 0 0 1
device=RESISTOR
T 43600 49400 5 10 1 1 0 0 1
refdes=R1
T 44300 50000 5 10 0 0 0 0 1
footprint=0603
T 43800 48900 5 10 1 1 0 0 1
value=1180
T 43700 48700 5 10 1 1 0 0 1
spec=0.1%
}
C 42800 44600 1 0 0 in-1.sym
{
T 42800 44900 5 10 0 0 0 0 1
device=INPUT
T 42800 44900 5 10 1 1 0 0 1
refdes=V+
}
C 52200 43800 1 0 1 in-1.sym
{
T 52200 44100 5 10 0 0 0 6 1
device=INPUT
T 52200 44100 5 10 1 1 0 6 1
refdes=SCLK
}
C 52200 43400 1 0 1 in-1.sym
{
T 52200 43700 5 10 0 0 0 6 1
device=INPUT
T 52100 43200 5 10 1 1 0 6 1
refdes=\_CS\_
}
N 50100 43900 51600 43900 4
N 51600 43500 50100 43500 4
C 43800 40600 1 0 0 in-1.sym
{
T 43800 40900 5 10 0 0 0 0 1
device=INPUT
T 43800 40900 5 10 1 1 0 0 1
refdes=GND
}
C 44300 40400 1 0 0 gnd-1.sym
L 41600 46300 42100 46300 3 0 0 0 -1 -1
L 42100 46300 42000 46400 3 0 0 0 -1 -1
L 42100 46300 42000 46200 3 0 0 0 -1 -1
L 41600 44700 42100 44700 3 0 0 0 -1 -1
L 42100 44700 42000 44800 3 0 0 0 -1 -1
L 42100 44700 42000 44600 3 0 0 0 -1 -1
V 41604 43893 392 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
L 41600 46300 41600 44300 3 0 0 0 -1 -1
L 41600 43500 41600 42500 3 0 0 0 -1 -1
L 41500 42400 41700 42400 3 0 0 0 -1 -1
L 41450 42500 41750 42500 3 0 0 0 -1 -1
L 41550 42300 41650 42300 3 0 0 0 -1 -1
T 41400 43800 9 10 1 0 0 0 1
RTD
T 40400 45100 9 10 1 0 0 0 2
External
connections
T 50200 41200 9 20 1 0 0 1 1
Temperature Measurement
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
C 53400 44000 1 0 0 Temperature.sym
{
T 56000 47600 5 10 1 1 0 3 1
refdes=X?
T 54500 45600 5 10 0 1 0 0 1
graphical=1
}
C 47600 42600 1 0 0 128S102.sym
{
T 48900 44000 5 10 1 1 0 3 1
refdes=U2
}
N 47600 44500 47600 42600 4
N 47600 42600 48500 42600 4
C 48400 42300 1 0 0 gnd-1.sym
C 49200 42300 1 0 0 gnd-1.sym
C 50200 44000 1 0 0 gnd-1.sym
N 50300 44300 50100 44300 4
N 51500 44700 50100 44700 4
N 49900 46400 49900 47300 4
C 47400 46000 1 0 0 gnd-1.sym
N 48600 45400 48600 49000 4
C 49000 47200 1 0 0 resistor.sym
{
T 49300 47600 5 10 0 0 0 0 1
device=RESISTOR
T 49200 47500 5 10 1 1 0 0 1
refdes=R6
T 49900 48100 5 10 0 0 0 0 1
footprint=1210
T 49400 47000 5 10 1 1 0 0 1
value=100
}
C 48100 49600 1 0 0 in-1.sym
{
T 48100 49900 5 10 0 0 0 0 1
device=INPUT
T 48100 49900 5 10 1 1 0 0 1
refdes=+3.3
}
N 49000 47300 49000 49700 4
N 49000 49700 48700 49700 4
C 47500 46900 1 0 0 in-1.sym
{
T 47500 47200 5 10 0 0 0 0 1
device=INPUT
T 47500 47200 5 10 1 1 0 0 1
refdes=+5
}
N 48600 47000 48100 47000 4
N 48400 46300 48600 46300 4
C 49500 45600 1 0 0 SMBT2222-1.sym
{
T 50000 46050 5 10 1 1 0 0 1
refdes=Q1
}
C 48600 45900 1 0 0 resistor.sym
{
T 48900 46300 5 10 0 0 0 0 1
device=RESISTOR
T 48800 46200 5 10 1 1 0 0 1
refdes=R7
T 49000 45700 5 10 1 1 0 0 1
value=10k
T 48600 45900 5 10 0 1 0 0 1
footprint=0603
T 48600 45900 5 10 0 1 0 0 1
spec=5% 1/10W
}
C 49900 45200 1 0 0 DoubleCap.sym
{
T 50100 45700 5 10 1 1 0 0 1
refdes=C2
}
C 50700 45100 1 0 0 gnd-1.sym
N 49900 45400 49900 45600 4
N 49900 45400 49200 45400 4
C 44300 42500 1 0 0 resistor.sym
{
T 44600 42900 5 10 0 0 0 0 1
device=RESISTOR
T 44500 42800 5 10 1 1 0 0 1
refdes=R8
T 44600 42300 5 10 1 1 0 0 1
value=1k
T 44300 42500 5 10 0 1 0 0 1
footprint=1210
T 44300 42500 5 10 0 1 0 0 1
spec=5% 1/2W
}
C 45200 42400 1 0 0 DoubleCap.sym
{
T 45400 42900 5 10 1 1 0 0 1
refdes=C1
}
C 46000 42300 1 0 0 gnd-1.sym
C 43700 42500 1 0 0 in-1.sym
{
T 43700 42800 5 10 0 0 0 0 1
device=INPUT
T 43700 42800 5 10 1 1 0 0 1
refdes=+15
}
C 45000 41500 1 0 0 LT1078S8-pwr.sym
{
T 45700 42300 5 10 0 0 0 0 1
device=LT1078IS8
T 45500 41900 5 10 1 1 0 0 1
refdes=U1
T 45700 42900 5 10 0 0 0 0 1
symversion=0.1
T 46000 42100 5 10 0 0 0 0 1
footprint=SO8
}
N 45200 42300 45200 42600 4
C 45300 49600 1 180 0 LT1078S8.sym
{
T 44600 48800 5 10 0 0 180 0 1
device=LT1078IS8
T 44600 49000 5 10 1 1 180 0 1
refdes=U1
T 44600 48200 5 10 0 0 180 0 1
symversion=0.1
T 44300 49000 5 10 0 0 180 0 1
footprint=SO8
}
C 45100 41200 1 0 0 gnd-1.sym
C 44600 46900 1 180 1 LT1078S8.sym
{
T 45300 46100 5 10 0 0 180 6 1
device=LT1078IS8
T 45300 46300 5 10 1 1 180 6 1
refdes=U1
T 45300 45500 5 10 0 0 180 6 1
symversion=0.1
T 45600 46300 5 10 0 0 180 6 1
footprint=SO8
T 45100 48400 5 10 0 0 0 0 1
slot=2
}
N 44600 46700 44600 47100 4
N 44600 47100 45600 47100 4
N 45600 47100 45600 46500 4
N 44600 46300 43400 46300 4
N 44300 49200 44300 50200 4
C 46600 48900 1 0 0 resistor.sym
{
T 46900 49300 5 10 0 0 0 0 1
device=RESISTOR
T 46800 49200 5 10 1 1 0 0 1
refdes=R3
T 47500 49800 5 10 0 0 0 0 1
footprint=0603
T 47000 48700 5 10 1 1 0 0 1
value=100k
T 46900 48500 5 10 1 1 0 0 1
spec=0.1%
}
N 47500 49000 48600 49000 4
C 45700 46400 1 0 0 resistor.sym
{
T 46000 46800 5 10 0 0 0 0 1
device=RESISTOR
T 45900 46700 5 10 1 1 0 0 1
refdes=R2
T 46600 47300 5 10 0 0 0 0 1
footprint=0603
T 46100 46200 5 10 1 1 0 0 1
value=100k
T 46000 46000 5 10 1 1 0 0 1
spec=0.1%
}
N 45700 46500 45600 46500 4
N 46600 46500 46600 49000 4
N 46600 49000 45300 49000 4
C 45300 50100 1 0 0 resistor.sym
{
T 45600 50500 5 10 0 0 0 0 1
device=RESISTOR
T 45500 50400 5 10 1 1 0 0 1
refdes=R5
T 46200 51000 5 10 0 0 0 0 1
footprint=0603
T 45600 49900 5 10 1 1 0 0 1
value=100k
T 45600 49700 5 10 1 1 0 0 1
spec=0.1%
}
C 44300 50100 1 0 0 resistor.sym
{
T 44600 50500 5 10 0 0 0 0 1
device=RESISTOR
T 44500 50400 5 10 1 1 0 0 1
refdes=R4
T 45200 51000 5 10 0 0 0 0 1
footprint=0603
T 44700 49900 5 10 1 1 0 0 1
value=100k
T 44600 49700 5 10 1 1 0 0 1
spec=0.1%
}
N 45300 49400 45300 50200 4
N 45300 50200 45200 50200 4
C 46100 49900 1 0 0 gnd-1.sym
C 47500 46100 1 0 0 DoubleCap.sym
{
T 47700 46600 5 10 1 1 0 0 1
refdes=C3
}
