#!/usr/bin/perl
# Reads OUTCAR, and POSCAR to print the maximum forces on all atoms by default
use warnings;
use strict;
use Getopt::Long;

sub trimsplit($){ # http://www.somacon.com/p114.php
    my $string = shift;
    chomp $string;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    my @array = split(/\s+/,$string);
    return @array;
}

my $printall = '';
GetOptions ('all' => \$printall);

print "\nthis script reads OUTCAR and POSCAR\n";
print "enable long form output with --all\n\n";

my @temp;

open(OUT,"OUTCAR") or die "error opening file: $!\n";
while (<OUT>){ last if /NIONS/ };
@temp = trimsplit $_;
my $nions = $temp[$#temp];
print "NIONS = $nions\n";

my @label;
push @label, 0;

open(POS,"POSCAR") or die "error opening file: $!\n";
my @poscar = <POS>;
close POS;
my @atoms  = trimsplit $poscar[5];
my @number = trimsplit $poscar[6];
print @atoms." atomic species:\n";
for(my $i = 0; $i < @atoms; $i++) { 
    print "  $atoms[$i] $number[$i] atoms\n";
    for my $atom (1..$number[$i]) {
	push @label, $atoms[$i];
    }
}

my @selective;
if ($poscar[7] =~ /select/i){
    print $poscar[7];
    foreach my $atom (1..$nions){
	#print $poscar[8+$nions];
	@temp = trimsplit $poscar[8+$atom];
	#print "$atom @temp[3..5] \n";
	foreach my $i (3..5){
	    if ($temp[$i] =~ /T/){
		push (@selective, 1);
	    }
	    elsif ($temp[$i] =~ /F/){
		push (@selective, 0);
		print "ion $atom constrained\n";
	    }
	    else {die;}
	}
    }
}
else{
    foreach my $atom (1..$nions){
	foreach my $i (3..5){
	    push (@selective, 1);
	}
    }
}

my $iteration;
while (my $line = <OUT>){
    if ($line =~ /Iteration/){ 
	$iteration = $line; 
    }
    elsif ($line =~ /TOTAL-FORCE/) {
	if ($printall) {print $iteration;}
	#print $line;
	<OUT>;
	my $forcemax = 0;
	my $forcemaxind = -1;
	foreach my $atom (1..$nions) {
	    $line = <OUT>;
	    @temp = trimsplit $line;
	    my $force = 0;
	    my $isconstrained = 0;
	    foreach my $i (3..5){
		$force += $temp[$i]**2 * $selective[3*($atom-1)+$i-3];
		$isconstrained    += 1 - $selective[3*($atom-1)+$i-3];
	    }
	    $force = sqrt $force;
	    if ($force > $forcemax){
		$forcemax = $force;
		$forcemaxind = $atom;
	    }
	    if ($printall) {
		print sprintf("%4u", $atom);
		print sprintf("%3s", $label[$atom]);
		print substr($line, 0, 45);
		print sprintf("F = %.3f", 1e3*$force);
		print "  constr." if $isconstrained;
		print "\n";
	    }
	}
	$forcemax = sprintf("%.3f", 1e3*$forcemax);
	$iteration =~ tr/-//d;
	chomp $iteration;
	print "$iteration max force  $forcemax meV/A  atom $forcemaxind  $label[$forcemaxind]\n";
    }
}
