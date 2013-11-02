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

$file_name_out=">poiseuille".$t.".dat";

open file_n, $file_name_in;
open file_a, $file_name_out;


$epsilon = 1.0e-18;
$pi      = 3.14159265359;
$mu  = 8.46;
$L   = 1.5;
$rho = 1.0;
$F   = 6.0;
$temp = 0;
$n    = 100;

while($line_n=<file_n>)
{
    
    @data_n = split(' ', $line_n);
    
    $y = $data_n[1];
    
    $Vx = $F/(2.0*$mu) * $y *($y-$L);
    
    $n = 0;
    $temp=0.0;
    do
    {
	$temp = 4.0*$F*($L**2)/( ($mu*($pi**3)) * (2*$n + 1)**3 );
	$temp = $temp * sin($pi * $y / $L * (2*$n+1));
	$temp = $temp * exp( -(2*$n+1)**2*($pi**2)*$mu *$t/($L**2) );
	
	$Vx = $Vx + $temp;	
	$n = $n +1;
    }
    while($temp > $epsilon);

    #print $y, ' ', -$Vx, "\n";
    print file_a  $y,' ', -$Vx, "\n";
}

close(file_n);
close(file_a);


