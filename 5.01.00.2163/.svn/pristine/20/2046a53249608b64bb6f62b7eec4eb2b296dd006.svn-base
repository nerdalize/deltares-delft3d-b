#! /usr/bin/perl

#-------------------------------------------------------------------------------
#   Filter to convert an old-style DD-bounds file to DDD input file (XML).
#   Optional command-line argument is number of nodes in cluster (default 1).
#   Domains and mappers are distributed over the nodes using round robin. 
#
#   Irv.Elshoff@deltares.nl
#   5 nov 05
#-------------------------------------------------------------------------------


use strict;
use warnings;


my $nodes = $ARGV[0] || 1;
($nodes < 1 || $nodes > 9999) and die "Improper number of nodes: $ARGV[0]\n";


#-----  Read DD-bound file

my %domains = ();
my %mappers = ();

while (<STDIN>) {
    chomp;
    my ($ldom, $l1, $l2, $l3, $l4, $rdom, $r1, $r2, $r3, $r4) = split;
    $ldom =~ s/.grd$//;
    $rdom =~ s/.grd$//;
    $domains{$ldom}++;
    $domains{$rdom}++;
    $mappers{"$ldom $l1 $l2 $l3 $l4 $rdom $r1 $r2 $r3 $r4"}++;
    }


#-----  Emit XML DDD file

print <<EOF;
<?xml version="1.0" encoding="iso-8859-1"?>
<delft3d>
    <cluster nodes="$nodes"/>
    <flow>
EOF

my $nextnode = 0;
foreach my $domain (sort keys %domains) {
    $nextnode++;
    printf "        <domain name=\"$domain\" node=\"$nextnode\"/>\n";
    $domains{$domain} = $nextnode;
    $nextnode %= $nodes;
    }

foreach my $mapper (sort keys %mappers) {
    my ($ldom, $l1, $l2, $l3, $l4, $rdom, $r1, $r2, $r3, $r4) = split / /, $mapper;
    print "        <couple name=\"$ldom=$rdom\" node=\"$domains{$ldom}\">\n";
    print "            <boundary domain=\"$ldom\">$l1 $l2 $l3 $l4</boundary>\n";
    print "            <boundary domain=\"$rdom\">$r1 $r2 $r3 $r4</boundary>\n";
    print "        </couple>\n";
    }


print <<EOF;
    </flow>
</delft3d>
EOF


