#print $ARGV[0];
#print $ARGV[1];

$ARGV[1] = '>>' . $ARGV[1];
open file_in, $ARGV[0];
open file_out, $ARGV[1];

while($line = <file_in>)
{
 
    @data_in = split(' ', $line);

    if ( $data_in[7] <=0 )
    {
	$v2=$data_in[2]**2+$data_in[3]**2;
	print file_out $data_in[0], ' ', $data_in[1], ' ', $v2, "\n";

    }
}

print $ARGV[1], "\n";

close(file_in);
close(file_out);
