#----------------------------------
# Generate a file which contains
# theoretical solution of poiseuille
# flow, Morris J.P. et al. 1997.
# The discrete points position is
# regular grid.
#----------------------------------
$epsilon = 1.0e-18;
$pi      = 3.14159265359;

$npart = 20;
$t     = 0.1;
$step  = $ARGV[0];
$dt    = $ARGV[1];
$t=$step*$dt;

#print $t, "  ", $dt, " ", $step, "\n";

$mu  = 8.46;
$L   = 1.5;
$rho = 1.0;
$F   = 750;
$dh   = $L /$npart;
$temp = 0;
$n    = 0;
#$timestring = substr($t, 2);
#$filename = "poiseuille_grid" . '_'.  $timestring .  ".dat";
$filename = "poiseuille_grid" . '_'.  $step .  ".dat";
open file,'>',$filename or die ;


for ($y = $dh/2.0; $y < $L; $y=$y+$dh)
{
    
    $Vx = $F/(2.0*$mu) * $y *($y-$L);
    
    $n = 0;
    $temp=0.0;
    do
    {
	$temp = 4.0*$F*($L**2)/( ($mu*($pi**3)) * (2*$n + 1)**3 );
	$temp = $temp * sin($pi * $y / $L * (2*$n+1));
	$temp = $temp * exp( -(2*$n+1)**2*($pi**2)*$mu *$t/($L**2) );
	if ($temp > $epsilon)
	{
	    $Vx = $Vx + $temp;
	}
	$n = $n +1;
    }
    while($temp > $epsilon);

    print file $y,' ', -$Vx, "\n";
}

    close(file);

