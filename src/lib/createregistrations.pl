#!/usr/bin/perl -w
use strict;

foreach my $filename (@ARGV) {
    $filename =~ m/^(?:.*\/)?(.+)\.pas$/os or die "$0: $filename: not a .pas file\n";
    my $include = "registrations/$1.inc";
    open(INFILE, '<', $filename) or die "$0: $filename: $!\n";
    my @registrations = ();
    my $foundInclude = 0;
    my $lastClass = undef;
    foreach (<INFILE>) {
        m/^ *(T[A-Za-z0-9_]+) *= *class/os && do {
            $lastClass = $1;
            # this is a seriously hacky hack
        };
        m/\@(Register[A-Za-z0-9_]*Class)/os && do {
            my $func = $1;
            m/^ *(T[A-Za-z0-9_]+) *= *class/os or die "$0: $filename: $.: cannot find class name\n";
            my $class = $1;
            push(@registrations, "   $func($class);\n");
            next;
        };
        m/\@(Register[A-Za-z0-9_]*Method)/os && do {
            my $func = $1;
            m/^ *(?:procedure|function) ([A-Za-z0-9_]+)\(/os or die "$0: $filename: $.: cannot find method name\n";
            die "$0: $filename: $.: haven't seen a class yet\n" unless defined $lastClass;
            my $method = $1;
            push(@registrations, "   $func(\@$lastClass.$method, TypeInfo(\@$lastClass.$method));\n");
            next;
        };
        m/{\$INCLUDE $include}/s && do {
            $foundInclude = 1;
        };
    }
    if (not @registrations) {
        if (-f $include) {
            print "$0: $filename: registrations removed; deleting $include\n";
            unlink $include or die "$0: $include: $!\n";
        }
        next;
    }
    if (not $foundInclude) {
        die "$0: $filename: wrong or missing registration include; expected {\$INCLUDE $include}\n";
    }
    close(INFILE);
    open(OUTFILE, '>', $include);
    local $" = '';
    print OUTFILE "@registrations";
    close(OUTFILE);
}
