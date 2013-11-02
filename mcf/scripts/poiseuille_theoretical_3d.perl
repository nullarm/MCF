#----------------------------------
# Generate a file which contains
# theoretical solution of poiseuille
# flow, Morris J.P. et al. 1997.
# The discrete points position is
# from input thin.dat.
#----------------------------------
#default input file name and time
$file_name_in="thin.dat";
$t   = 10;

$file_name_in=$ARGV[0];
$t=$ARGV[1];

$file_name_out=">poiseuille_particle".$t.".dat";

open file_in, $file_name_in;
open file_out, $file_name_out;


$epsilon = 1.0e-18;
$pi      = 3.14159265359;
$mu  = 8.46;
$L   = 1.5;
$rho = 1.0;
$F   = 750.0;
$temp = 0;
$n    = 100;

while($line_in=<file_in>)
{
    
    @data_in = split(' ', $line_in);
    
    $z = $data_in[0];
    
    if ($z>$L) {next;}

    $Vx = $F/(2.0*$mu) * $z *($z-$L);
    
    $n = 0;
    $temp=0.0;
    do
    {
	$temp = 4.0*$F*($L**2)/( ($mu*($pi**3)) * (2*$n + 1)**3 );
	$temp = $temp * sin($pi * $z / $L * (2*$n+1));
	$temp = $temp * exp( -(2*$n+1)**2*($pi**2)*$mu *$t/($L**2) );
	
	$Vx = $Vx + $temp;	
	$n = $n +1;
    }
    while($temp > $epsilon);

    #print $z, ' ', -$Vx, "\n";
    print file_out  $z,' ', -$Vx, "\n";
}

close(file_in);
close(file_out);


