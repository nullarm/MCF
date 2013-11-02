#----------------------------------
# Generate a file which contains
# theoretical solution of poiseuille
# flow
# Morris J.P. et al. 1997 has typo
# We use Sigalotti et al. 2003.
# The discrete points position is
# regular grid.
#----------------------------------
$epsilon = 1.0e-30;
$pi      = 3.14159265359;

$nmax  = 1000;
$npart = 20;
$t     = 0.1;
$step  = $ARGV[0];
$dt    = $ARGV[1];
$t=$step*$dt;
$npart = $ARGV[2];

#print $t, "  ", $dt, " ", $step, "\n";

$mu  = 8.46;
$L   = 1.5;
#$L   = 3;
$d   = $L/2.0;
$rho = 1.0;
$F   = 750;#force per unit mass
#$F   = 187.5;
$dh   = $L/$npart;
$temp = 0;
$n    = 0;
#$timestring = substr($t, 2);
#$filename = "poiseuille_grid" . '_'.  $timestring .  ".dat";
$filename = "poiseuille_grid" .  $step .  ".dat";

open file,'>',$filename or die ;


for ($y =-$d+$dh/2.0; $y < $d; $y=$y+$dh)
{
    
    $vx = $F/(2.0*$mu) * ($y**2-$d**2);
    
    $n=0;
    $temp=0.0;
    do
    {
	$temp1 = 16.0*$F*$d**2/$mu/$pi**3/(2*$n+1)**3;
	$temp2 = cos($pi * $y * (2*$n+1)/2.0/$d);
	$temp3 = exp( -(2.0*$n+1)**2*($pi**2)*$mu*$t/4.0/($d**2) );
	$temp  = $temp1*$temp2*$temp3;
	
	$temp_abs=abs($temp);
	
	if ($temp_abs > $epsilon)
	{
	    if ($n%2==1) 
	    {		
		$vx = $vx - $temp;
		#print "odd\n";
	    }
	    else
	    {
		$vx = $vx + $temp;
	    }	
	}
	$n = $n + 1;
    }
    while($temp_abs > $epsilon);
    #print "y,n,temp2,temp3,temp, vx: ", $y, " ", $n, " ", $temp2, " ", $temp3, " ", $temp, " ", -$vx, "\n";
    print file $y+$d,' ', -$vx, "\n";
}

close(file);

