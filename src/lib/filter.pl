#!/usr/bin/perl -wT
use strict;

my $error = 0;
my $sizeMismatch = 0;
my $nonimpl = 0;
while (<>) {
    chomp;
    next if m!^Hint: (?:Start|End) of reading config file /home/$ENV{USER}/\.fpc\.cfg$!os;
    next if m!^Free Pascal Compiler version 2\.7\.1 \[[-/0-9]+] for x86_64$!os;
    next if m!^Copyright \(c\) 1993-.... by Florian Klaempfl and others$!os;
    next if m!^Target OS: Linux for x86-64$!os;
    next if m!^Compiling .+$!os;
    next if m!^/usr/bin/ld: warning: \.\./bin/link\.res contains output sections; did you forget -T\?$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Parameter ".+" not used$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Value parameter ".+" is assigned but never used$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Local variable "\$self" does not seem to be initialized$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Warning: Constructor should be public$!os; # i'll use whatever visibility i want, thanks
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Inlining disabled$!os;
    next if m!^Error: .+ppcx64 returned an error exitcode \(normal if you did not specify a source file to be compiled\)$!os;
    next if m!^Linking ../bin/[-a-z_0-9]+$!os;
    next if m!^[0-9]+ lines compiled, [0-9.]+ sec *$!os;
    next if m!^[0-9]+ (?:hint|warning|note)\(s\) issued$!os;

    if (m!Warning: Symbol "XXX" is not implemented!os) {
        $nonimpl += 1;
        next if $nonimpl > 5;
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) Warning: (?:Comparison might be always true due to range of constant and expression|unreachable code)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        open(FILE, $file) or die "could not open $file: $!\n";
        local $_;
        <FILE> for (1..$line-1);
        my $statement = <FILE>;
        close(FILE);
        next if $statement =~ m/^ *Assert\(/os;
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) Hint: Conversion between ordinals and pointers is not portable$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        open(FILE, $file) or die "could not open $file: $!\n";
        local $_;
        <FILE> for (1..$line-1);
        my $statement = <FILE>;
        close(FILE);
        next if $statement =~ m/PtrUInt\(/os;
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) ((?:Warning|Hint|Note): .+)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        my $message = $4;
        open(FILE, $file) or die "could not open $file: $!\n";
        local $_;
        <FILE> for (1..$line-1);
        my $statement = <FILE>;
        close(FILE);
        next if $statement =~ m! {BOGUS \Q$message\E}(?: //.+)?\n$!s;
        next if $statement =~ m! // http://bugs\.freepascal\.org/view\.php\?id=25916\n$!s; # DFA gives bogus message about "var" parameter of type "Text" not being initialized
        next if $statement =~ m! // http://bugs\.freepascal\.org/view\.php\?id=26402\n$!s; # Private type "<record type>...." never used
        next if $statement =~ m! // http://bugs\.freepascal\.org/view\.php\?id=26403\n$!s; # DFA gives bogus message about dynamic arrays being uninitialized
        next if $statement =~ m! // http://bugs\.freepascal\.org/view\.php\?id=25914\n$!s; # DFA gives bogus message: Hint: Local variable "$self" does not seem to be initialized
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) (?:Warning|Hint): (?:Type size mismatch, possible loss of data / range check error)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        open(FILE, $file) or die "could not open $file: $!\n";
        my @lines = <FILE>;
        close(FILE);
        if ($lines[$line-1] =~ m!// \$R-!s or
            $lines[$line-1] =~ m!// http://bugs\.freepascal\.org/view.php\?id=25703!s) {
            # hide this warning
            next;
        } elsif ($lines[$line-2] =~ m!^ *if \(Length\((.+)\) > ([0-9]+)\) then\n$!os) {
            # High() can return a negative number if Length() = 0, but we've checked that Length > 0, so it's bogus
            my $expression = $1;
            my $min = $2;
            if ($lines[$line-1] =~ m!^ *for .+ := Low\(\Q$expression\E\) to High\(\Q$expression\E\) do\n$!s or
                $lines[$line-1] =~ m!^ *for .+ := High\(\Q$expression\E\) downto Low\(\Q$expression\E\) do\n$!s or
                $lines[$line-1] =~ m!^ *for .+ := 1 to Length\(\Q$expression\E\) do\n$!s) {
                next;
            }
        } elsif ($lines[$line-1] =~ m!WriteCardinal\(Length\(!s) {
            # Length() can never return negative numbers, even though it's declared signed
            next;
        } else {
            $sizeMismatch += 1;
        }
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) Warning: Function (result) variable does not seem to initialized$!os or
        m!^([^(]+)\(([0-9]+),([0-9]+)\) Hint: Local variable "([^"]+)" does not seem to be initialized$!os or
        m!^([^(]+)\(([0-9]+),([0-9]+)\) Warning: Local variable "([^"]+)" does not seem to be initialized$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        my $variable = $4;
        open(FILE, $file) or die "could not open $file: $!\n";
        my @lines = <FILE>;
        close(FILE);
        # warn "COMPARING LINE FOR $variable: " . $lines[$line-1] . "\n";
        if ($lines[$line-1] =~ m!// \$DFA- for (?i:\Q$variable\E)!s or
            $lines[$line-1] =~ m!^ *Assert\(!s) {
            # hide this warning
            next;
        }
    }

    $error = 1 if m!^Fatal: Compilation aborted$!os;
    print "$_\n";
}
if ($nonimpl > 5) {
    print "There are $nonimpl places marked as unimplemented (first five shown above).\n";
}
if ($sizeMismatch) {
    print "There are $sizeMismatch places where there's a type size mismatch, possible loss of data / range check error\n";
}
exit 1 if $error;

#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Value parameter ".+" is assigned but never used$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Warning: Mixing signed expressions and longwords gives a 64bit result$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Converting the operands to "Int64" before doing the add could prevent overflow errors\.$!os;
