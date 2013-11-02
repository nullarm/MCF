#############################################################
# Calculate quantities of solvent velocity, including
# 1) PDF: probability distribution function
# 2) Fluctuation moments: M0,M1,M2,M3,M4.
#
# Revision date: Oct.3, 2013
# Remark,
# nth moment is averaged over all particles at each file,
# namly, each time step, then further averaged over
# all the files.
# This is different (for n>=2) to that
# all particles loaded in from all files/time steps and
# then averaged together.
#############################################################
use Math::Trig;

###################################################
# Default values
###################################################
$num_dim     = 3;
$step_start  = 10000;
$step_end    = 99999999;
$num_bin     = 30; # number of bins for PDF histogram
$num_extreme = 40;
$num_moment  = 4;

###################################################
# Values of parameters given by user
###################################################
$dir_name        = $ARGV[0];
$file_out_prefix = $ARGV[1];
$step_start      = $ARGV[2];
$step_end        = $ARGV[3];
$file_in_prefix  = "mcf_particles";
$num_file        = 0;

###################################################
# Open directory and sort files by name
###################################################
opendir(DIR, $dir_name) || 
die   ("can not open directory: ".$dir_name.", as it does not exist !\n");

print "processing directory  : ", $dir_name, "\n";
@file_names_temp = readdir(DIR);
@file_names_in   = sort @file_names_temp;
print "number of files inside: ", scalar(@file_names_in), "\n";
closedir(DIR);

###################################################
# Set bounds for PDF histogram.
# Set bin size.
# Set initial values.
# Set moments to zero
###################################################
for ($i=0;$i<$num_dim;$i++)
{
    $v_min[$i] = -$num_extreme;
    $v_max[$i] =  $num_extreme;
    $dv[$i]    = ($v_max[$i]-$v_min[$i])/$num_bin;
    
    for ($j=0;$j<$num_bin;$j++)
    {
	$v_hist[$i][$j] = $v_min[$i] + $j*$dv[$i] + 0.5*$dv[$i];
	$pdf_v[$i][$j] = 0.0;
    }    
}
###################################################
#sum of velocity squared
#speed
###################################################
$v2_min = 0.0;
$v2_max = $num_extreme**2;
$dv2    = ($v2_max-$v2_min)/$num_bin;
$vs_min = 0.0;
$vs_max = sqrt($v2_max);
$dvs    = ($vs_max-$vs_min)/$num_bin;

for ($j=0;$j<$num_bin;$j++)
{
    $v2_hist[$j] = $v2_min + $j*$dv2 + 0.5*$dv2;
    $pdf_v2[$j]  = 0.0;
    $vs_hist[$j] = $vs_min + $j*$dvs + 0.5*$dvs;
    $pdf_vs[$j]  = 0.0;
}


for ($i=0;$i<$num_dim;$i++)
{
    for ($k=0;$k<=$num_moment;$k++)
    {
	$M[$i][$k] = 0.0;
    }
    $sigma[$i]   = 0.0;
    $M2_check[$i]= 0.0;
}

#############################################################
# Loop over all files according to starting and ending files.
#############################################################
$num_file=0;

$f_start = $file_in_prefix . stepstring($step_start). ".out";
$f_end   = $file_in_prefix . stepstring($step_end). ".out";
print "starting file   : ", $f_start, "\n";
print "ending file     : ", $f_end, "\n";

foreach $f (@file_names_in)
{
    #print "checking file : ", $f, "\n";
    
    if (( $f ge $f_start) && ( $f le $f_end ) )
    {
	$file_name = $dir_name . $f;
	print "processing file : ", $file_name, "\n";

        ###########################################
        # Open a solvent data file
        # reset counter to zero. 
        # read each solvent particle's velocity
        ###########################################
	
	open (IN, $file_name);	
	
	for($i=0;$i<$num_dim;$i++)
	{
	    for($j=0;$j<$num_bin;$j++)
	    {
		$pdf_v_file[$i][$j]=0;
	    }
	    
	    for($k=0;$k<=$num_moment;$k++)
	    {
		$M_file[$i][$k]=0.0;
	    }
	}
	
	for ($j=0;$j<$num_bin;$j++)
	{
	    $pdf_v2_file[$j]=0.0;
	    $pdf_vs_file[$j]=0.0;
	}

	$num_p   = 0;
	$v2_file = 0.0;
	
	while ($line = <IN>)
	{
	    @data = split(' ', $line);

	    $v2_single = 0.0;
	    for($i=0;$i<$num_dim;$i++)
	    {
		$v[$i][$num_p] = $data[$num_dim+$i];

		$j             = ($v[$i][$num_p]-$v_min[$i])/$dv[i];
		$pdf_v_file[$i][$j]++;
		
		$v2_single    += $v[$i][$num_p]**2;		
	    }
	    
	    $j = ($v2_single-$v2_min)/$dv2;
	    $pdf_v2_file[$j]++;
	    $j = (sqrt($v2_single)-$vs_min)/$dvs;
	    $pdf_vs_file[$j]++;
	    
	    for($i=0;$i<$num_dim;$i++)
	    {
		$M_file[$i][0] += $v[$i][$num_p];
	    }
	    
	    $v2_file += $v2_single;

	    $num_p++;
	    
	} # finish reading this file
	
	close(IN);
	
	###########################################
	# Calculate average of data in this file
        # PDF and 0th order moment(arithmetic average)
	###########################################
	#print "num_p : ", $num_p, "\n";
	
	for($i=0;$i<$num_dim;$i++)
	{
	    for($j=0;$j<$num_bin;$j++)
	    {
		$pdf_v_file[$i][$j] /= $num_p;
	    }
	}
	
	for($j=0;$j<$num_bin;$j++)
	{
	    $pdf_v2_file[$j] /= $num_p;
	    $pdf_vs_file[$j] /= $num_p
	}

	for($i=0;$i<$num_dim;$i++)
	{
	    $M_file[$i][0] /= $num_p;

	}
	
	$v2_file /= $num_p;	

        ###########################################
        # Loop over all particles in this file
        # for the second time,
        # to calculate high-order(>0) moments
	###########################################
	
	for($j=0;$j<$num_p;$j++)
	{
	    for($i=0;$i<$num_dim;$i++)
	    {
		for($k=1;$k<=$num_moment;$k++)
		{
		    $M_file[$i][$k] += ($v[$i][$j]-$M_file[$i][0])**$k;
		}
	    }
	}
	
	for($i=0;$i<$num_dim;$i++)
	{
	    for($k=1;$k<=$num_moment;$k++)
	    {
		$M_file[$i][$k] /= $num_p;
	    }
	    #######################################
	    # Calculate standard deviation
	    #######################################
	    $sigma_file[$i] = sqrt($M_file[$i][2]);
	}
	
	for($i=0;$i<$num_dim;$i++)
	{
	    for($k=3;$k<=$num_moment;$k++)
	    {
		$M_file[$i][$k] /= $sigma_file[$i]**$k;
	    }
	}
	
        ###########################################
	# accumulate quantities in this file
        # increase counter of files
        ###########################################
	
	for($i=0;$i<$num_dim;$i++)
	{
	    for ($j=0;$j<$num_bin;$j++)
	    {
		$pdf_v[$i][$j] += $pdf_v_file[$i][$j];
	    }	
	    
	}
	
	for ($j=0;$j<$num_bin;$j++)
	{
	    $pdf_v2[$j] += $pdf_v2_file[$j];
	    $pdf_vs[$j] += $pdf_vs_file[$j];
	}	

	for($i=0;$i<$num_dim;$i++)
	{
	    for($k=0;$k<=$num_moment;$k++)
	    {
		$M[$i][$k] += $M_file[$i][$k];
	    }
	    $sigma[$i] += $sigma_file[$i];
	}
	
	$num_file ++;
	
    } # if file is in consideration.
    
}# folder


###################################################
# Normalize quantities with bin size and
# verage quantities over number of files.
###################################################
for($i=0;$i<$num_dim;$i++)
{
    for ($j=0;$j<$num_bin;$j++)
    {
	$pdf_v[$i][$j] /= $dv[$i];
	$pdf_v[$i][$j] /= $num_file;
    }    
}

for ($j=0;$j<$num_bin;$j++)
{
    $pdf_v2[$j] /= $dv2;
    $pdf_v2[$j] /= $num_file;
    $pdf_vs[$j] /= $dvs;
    $pdf_vs[$j] /= $num_file;
}


for($i=0;$i<$num_dim;$i++)
{
    for($k=0;$k<=$num_moment;$k++)
    {
	$M[$i][$k] /= $num_file;
    }
    $sigma[$i] /= $num_file;
}

print "number of files processed: ", $num_file, "\n";

print "pdf_v_0.dat: PDF of vx\n";
print "pdf_v_1.dat: PDF of vy\n";
print "pdf_v_2.dat: PDF of vz\n";
print "pdf_v_3.dat: PDF of (vx+vy+vz)\/3\n";
print "pdf_v2.dat : PDF of vx^2+vy^2+vz^2\n";
print "pdf_vs.dat : PDF of sqrt(vx^2+vy^2+vz^2)\n";

#Average three directions.
for ($j=0;$j<$num_bin;$j++)
{
    $pdf_v[3][$j]  = ($pdf_v[0][$j]+$pdf_v[1][$j]+$pdf_v[2][$j])/3.0;
    $v_hist[3][$j] = $v_hist[0][$j]; # just take x direction.
}
for ($i=0;$i<=$num_dim;$i++)
{
    $file_out_name = ">".$file_out_prefix."_".$i.".dat"; 
    open(OUT,$file_out_name);
    
    $unity[$i]=0.0;
    
    for ($j=0;$j<$num_bin;$j++)
    {
	print OUT $v_hist[$i][$j], ' ', $pdf_v[$i][$j], "\n";
	$unity[$i] += $pdf_v[$i][$j]*$dv[$i];
    }
    print "Unity check--->: sum of pdf_v[",$i,"][j] = ", $unity[$i], "\n";
    close(OUT);
}

$file_out_name = ">".$file_out_prefix."2".".dat"; 
open(OUT2,$file_out_name);
$file_out_name = ">".$file_out_prefix."s".".dat"; 
open(OUTs,$file_out_name);

$unity2=0.0;
$unitys=0.0;

for ($j=0;$j<$num_bin;$j++)
{
    print OUT2 $v2_hist[$j], ' ', $pdf_v2[$j], "\n";
    $unity2 += $pdf_v2[$j]*$dv2;
    print OUTs $vs_hist[$j], ' ', $pdf_vs[$j], "\n";
    $unitys += $pdf_vs[$j]*$dvs;
}
print "Unity check--->: sum of pdf_v2[j]   = ", $unity2, "\n";
print "Unity check--->: sum of pdf_vs[j]   = ", $unitys, "\n";

close(OUT2);
close(OUTs);

print "Fluctuation moments in three directions:\n";

for($k=0;$k<=$num_moment;$k++)
{
    print "M", $k,"------>: ";
    for ($i=0;$i<$num_dim;$i++)
    {
	printf("%s%u%s%15.8e%s", "[",$i, "] = ", $M[$i][$k], ' ');
    }
    print "\n";
}
print "sigma--->: ";
for ($i=0;$i<$num_dim;$i++)
{
    printf("%s%u%s%15.8e%s", "[",$i, "] = ", $sigma[$i], ' ');
}
print "\n";

sub stepstring
{

    $step=$_[0];
    $length_step=length($step);

    $string = $step;
    for ($i=$length_step;$i<8;$i++)
    {
	$string = "0" . $string;
    }

    return $string;
}
