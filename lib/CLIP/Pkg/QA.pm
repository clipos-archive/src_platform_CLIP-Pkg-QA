# SPDX-License-Identifier: LGPL-2.1-or-later
# Copyright © 2011-2018 ANSSI. All Rights Reserved.
package CLIP::Pkg::QA;

use 5.008008;
use strict;
use warnings;
use Sort::Versions;

use CLIP::Logger qw(:all);

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
clippkg_get_prio
clippkg_get_dist
clippkg_get_fuzzy_deps
clippkg_get_fuzzy_deplist
clippkg_get_strict_deps
clippkg_get_strict_deps_full
clippkg_add_provided
clippkg_check_paths
clippkg_check_category
clippkg_check_depends
clippkg_check_conflicts
clippkg_check_deps
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
);

our $VERSION = '1.0.2';
	
=head1 NAME

CLIP::Pkg::QA - Perl extension to manage QA checks on CLIP packages.

=head1 VERSION

Version 1.0.2

=head1 SYNOPSIS

  use CLIP::Pkg::QA;
  use CLIP::Pkg::QA ':all';

=head1 DESCRIPTION

CLIP::Pkg::QA provides basic functions to manipulate CLIP binary packages and 
mirrors for QA testing : field extraction and verification, path and dependency
checks, etc.

CLIP::Pkg::QA logs its outputs through the CLIP::Logger module.

=head1 EXPORT

The module does not export anything by default. It defines a single Exporter 
tag, ":all", which exports the following functions and variables:

=over 4

=item *

B<clippkg_get_prio>

=item *

B<clippkg_get_dist>

=item *

B<clippkg_get_strict_deps>

=item *

B<clippkg_get_strict_deps_full>

=item *

B<clippkg_add_provided>

=item *

B<clippkg_check_paths>

=item *

B<clippkg_check_category>

=item *

B<clippkg_check_depends>

=item *

B<clippkg_check_conflicts>

=item *

B<clippkg_check_deps>

=back

=cut

###############################################################
#                          SUBS                               #
###############################################################

=back

=head1 FUNCTIONS

CLIP::Pkg::Base provides the following functions:

=cut
		       
                       ################################
		       #      DPKG info extraction    #
		       ################################

=head2 DPKG info extraction

=over 4

=item B<clippkg_get_prio($deb)>

Returns the 'Priority' field value of package $deb (full path) or undef.

=cut

sub clippkg_get_prio($) {
	my $deb = shift;

	unless (open PRIO, "dpkg -f $deb Priority |") {
		clip_warn "Failed to run dpkg -f on $deb";
		return undef;
	}
	my $prio = <PRIO>;
	unless (close PRIO) {
		clip_warn "dpkg -f $deb failed";
		return undef;
	}
	chomp $prio;

	return lc($prio);
}

=item B<clippkg_get_dist($deb)>

Returns the 'Distribution' field value of package $deb (full path) or undef.

=cut

sub clippkg_get_dist($) {
	my $deb = shift;

	unless (open DIST, "dpkg -f $deb Distribution |") {
		clip_warn "Failed to run dpkg -f on $deb";
		return undef;
	}
	my $dist = <DIST>;
	close DIST;
	chomp $dist;

	return $dist;
}

=item B<clippkg_get_strict_deps($pkg, $arch, $field, $hash)>

Fills the hash referenced by $hash with the dependencies of package $pkg (full path)
and field $field (i.e. Depends, Suggests, etc...), for arch $arch.

The hash structure is as follows :
 <pname> => [ <pver>, <pfull>, <field> ]
 with <pname> the Package name, <pver> the version for that package, <pfull> the full name of
 that package, and <field> the Depends/Suggests field for the dependency that pulled that package
 in.

Note that this function only works on 'strict' dependencies, i.e. '= version', as in a configuration's
dependencies.

=cut

sub clippkg_get_strict_deps($$$$) {
	my ($pkg, $arch, $field, $hash) = @_;

	open PIPE, "dpkg -f \'$pkg\' $field 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "Failed to get $field dependencies on $pkg";
		foreach (@output) {
			clip_warn "dpkg output: $_";
		}
		return 0;
	}

	return 1 unless (defined ($output[0]));

	my @deps = split ",", $output[0];

	foreach my $dep (@deps) {
		if ($dep =~ /(\S+) \(= (\S+)\)/) {
			my $pname = $1;
			my $pver = $2;

			if (defined($hash->{$pname})) {
				clip_warn "Double dependency on $pname : $pver vs ".$hash->{$pname}->[0];
				return 0;
			}
			$hash->{$pname} = [ $pver, $pname."_".$pver."_".$arch.".deb", $field ]
		} else {
			clip_warn "Invalid dependency atom: $dep";
			return 0;
		}
	}
	return 1;
}

=item B<clippkg_get_strict_deps_full($pkg, $arch, $hash)>

Same as clippkg_get_strict_deps, except the dependencies are added for both 'Depends' and 'Suggests' 
dependencies.

=cut
sub clippkg_get_strict_deps_full($$$) {
	my ($deb, $arch, $hash) = @_;

	unless (clippkg_get_strict_deps($deb, $arch, "Depends", $hash)) {
		clip_warn "Failed to get Depends on $deb";
		return 0;
	}
	unless (clippkg_get_strict_deps($deb, $arch, "Suggests", $hash)) {
			clip_warn "Failed to get Suggests on $deb";
			return 0;
	}

	return 1;
}

=item B<CLIP::Pkg::QA::parse_fuzzy_deps($list, $pkg, $field, $hash)>

Internal use only. 
Parse a 'fuzzy' (i.e, not just strict - dependency constraints other 
than = are also supported) dependencies, expressed as a list of dependency
atoms referenced by $list, and add them to the hash referenced by $hash, with 
the following structure :
 <pname> => [<pver>, <prel>]
 where <pname> is the dependency package name, <pver> the dependency constraint version, 
 and <prel> the dependency constraint ('>>', '<=', '=', etc.). When no version constraint
 is associated to a dependency, <pver> and <prel> are stored as respectively '0' and '*'.

$field and $pkg are passed for logging only.

=cut

sub parse_fuzzy_deps($$$$) {
	my ($list, $pkg, $field, $hash) = @_;

	DEPLOOP:
	foreach my $dep (@{$list}) {
		if ($dep =~ /(\S+) \((\S+) (\S+)\)/) {
			my $pname = $1;
			my $prel = $2;
			my $pver = $3;

			if (defined($hash->{$pname})) {
				clip_warn "   Note: $pkg has a double $field dependency on $pname : $pver vs ".$hash->{$pname}->[0];
			}
			$hash->{$pname} = [ $pver, $prel ]
		} elsif ($dep =~ /^\s*[^\(\s\)]+\s*$/) {
			# Simple dep, no version
			chomp $dep;
			$dep =~ s/\s//g;
			if (defined($hash->{$dep})) {
				clip_warn "   Note: $pkg has a double $field dependency on $dep";
				next DEPLOOP;
			}
			$hash->{$dep} = [ 0, '*' ]
		} else {
			clip_warn "Invalid dependency atom: $dep";
			return 0;
		}
	}
	return 1;
}

=item B<CLIP::Pkg::QA::clippkg_get_fuzzy_deps($pkg, $field, $hash)>

Store the 'fuzzy' dependencies of type $field (Replaces, Conflicts, whatever) for $pkg into the single
hash referenced by $hash.
This only works when no alternatives are present, i.e. each dependency atom references a single package,
and not e.g. 'foo|bar'.

=cut

sub clippkg_get_fuzzy_deps($$$) {
	my ($pkg, $field, $hash) = @_;

	open PIPE, "dpkg -f \'$pkg\' $field 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "Failed to get $field dependencies on $pkg";
		foreach (@output) {
			clip_warn "dpkg output: $_";
		}
		return 0;
	}

	return 1 unless (defined ($output[0]));

	my @deps = split ",", $output[0];
	
	return parse_fuzzy_deps(\@deps, $pkg, $field, $hash);
}

=item B<CLIP::Pkg::QA::clippkg_get_fuzzy_deplist($pkg, $field, $list)>

Store the 'fuzzy' dependencies of type $field (Replaces, Conflicts, whatever) for $pkg into a list of
hash references, itself referenced by $list.
This store every dependency atom in a single hash, and supports alternatives, i.e. an atom like
foo|bar (>> 2.0) gets stored into the list as a reference to hash :
	[ 
		foo 	=>	[ 0, 	'*' ],
		bar	=>	[ 2.0,	'>>' ],
	]
=cut

sub clippkg_get_fuzzy_deplist($$$) {
	my ($pkg, $field, $list) = @_;

	open PIPE, "dpkg -f \'$pkg\' $field 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "Failed to get $field dependencies on $pkg";
		foreach (@output) {
			clip_warn "dpkg output: $_";
		}
		return 0;
	}
	return 1 unless (defined ($output[0]));

	my @deps = split ',', $output[0];

	foreach my $atom (@deps) {
		my @alts = split '\|', $atom;
		my %hash = ();
		return 1 unless (parse_fuzzy_deps(\@alts, $pkg, $field, \%hash));
		push @{$list}, (\%hash);
	}
	return 1;
}

=item B<clippkg_add_provided($hash)>

This completes a hash of strict dependencies, as returned by clippkg_get_strict_deps(), 
by adding to it all the virtual packages provided by the non-virtual packages already in
the hash. Virtual packages are added with a version of '0' and a full name of '-'.
For this function to work, all the non-virtual packages referenced in $hash must be 
present in the current directory.

=cut

sub clippkg_add_provided($) {
	my $hash = shift;

	foreach my $pname (keys %{$hash}) {
		my %prov = ();
		return 0 unless (clippkg_get_fuzzy_deps($hash->{$pname}->[1], 'Provides', \%prov));
		foreach my $pvname (keys %prov) {
			$hash->{$pvname} = [ 0, '-', $hash->{$pname}->[2]]
				unless (defined($hash->{$pvname}));
		}
	}

	return 1;
}
=back

=cut
                       ################################
		       #  Path and priority checks    #
		       ################################

=head2 Path checks

=over 4

=item B<CLIP::Pkg::QA::check_path_primary($deb, $dist)>

Internal use only. 
Check the paths of files in package $deb (full path), and 
make sure they are acceptable for a primary package in
distribution $dist.
Returns 0 if some incorrect paths are found, 1 otherwise.

=cut
sub check_path_primary($$) {
	my ($deb,$dist) = @_;
	my $ret = 1;

	open PLIST, "dpkg -c $deb | awk '{print \$6}' |" 
		or die "Failed to run dpkg -c on $deb";
	foreach my $path (<PLIST>) {
		if ($path =~ /\.\/usr\/local\//) {
			chomp $path;
			next if ($path eq './usr/local/'); # we allow empty dir
			clip_warn "Incorrect path in $deb: $path";
			$ret = 0;
		}
	}
	close PLIST;

	return $ret;
}

=item B<CLIP::Pkg::QA::check_path_secondary($deb, $dist)>

Internal use only. 
Check the paths of files in package $deb (full path), and 
make sure they are acceptable for a secondary package in
distribution $dist.
Returns 0 if some incorrect paths are found, 1 otherwise.

=cut

sub check_path_secondary($$) {
	my ($deb,$dist) = @_;
	my $ret = 1;

	open PLIST, "dpkg -c $deb | awk '{print \$6}' |" 
		or die "Failed to run dpkg -c on $deb";
	foreach my $path (<PLIST>) {
		if ($path =~ /^\.\/usr\/local\//) {
			$path =~ /^\.\/usr\/local\/local/ or next;
			chomp $path;
			clip_warn "Double local path in $deb: $path";
			return 0;
		}
		$path =~ /^\.\/var\// and next;
		if ($dist eq 'clip') {
			$path =~ /^\.\/mounts\// and next;
			$path =~ /^\.\/viewers\// and next;
			$path =~ /^\.\/etc\/$/ and next;
			$path =~ /^\.\/etc\/admin\// and next;
		} elsif ($dist eq 'rm') {
			$path =~ /^\.\/audit_root\// and next;
			$path =~ /^\.\/user_root\// and next;
		}
		chomp $path;
		next if ($path eq './' or $path eq './usr/');

		clip_warn "Incorrect prefix path in $deb: $path";
		$ret = 0;
	}
	close PLIST;

	return $ret;
}

=item B<clippkg_check_paths($deb, $dist, $prio)>

Check the paths of files installed by package $deb (full path), make
sure they are acceptable for a package of priority $prio in distribution
$dist.
Returns 0 if some incorrect paths are found, 1 otherwise.

=cut

sub clippkg_check_paths($$$) {
	my ($deb, $dist, $prio) = @_;

	if ($prio eq 'required') {
		return 0 unless check_path_primary($deb, $dist);
	} elsif ($prio eq 'important') {
		return 0 unless check_path_secondary($deb, $dist);
	} else {
		clip_warn "Unsupported priority in $deb: $prio";
		return 0;
	}
}

=item B<clippkg_check_category($pkg, $dist, $prio)>

Perform full checks on package $pkg by :
 - making sure it has priority $prio in the $dist distribution
 - checking the paths of the files it installs for those same
   priority and distribution

Return 1 if everything is OK, 0 otherwise.

=cut

sub clippkg_check_category($$$) {
	my ($deb, $p_dist, $p_prio) = @_;

	my $prio = clippkg_get_prio($deb);
	my $dist = clippkg_get_dist($deb);

	if ($p_prio ne "" and $prio ne $p_prio) {
		clip_warn "Incoherent priority for dependency $deb: $prio != $p_prio";
		return 0;
	}
	if ($p_dist ne "" and $dist ne $p_dist) {
		clip_warn "Incoherent distribution for dependency $deb: $dist != $p_dist";
		return 0;
	}
	
	return clippkg_check_paths($deb, $dist, $prio);

	return 1;
}
=back

=cut
                       ############################
		       #     Dependency checks    #
		       ############################

=head2 Dependency checks

=over 4

=item B<CLIP::Pkg::QA::check_one_dep($dep, $candidate)>

Internal use only. 
Checks that candidate $candidate ([<version>, <full name>, <type>] as the values
of a strict dependencies hash) matches the dependency $dep ([<version>, <constraint>] as
the values of a fuzzy dependencies hash), assuming both refs refer to the same package
name.
Return 1 if the dependency is satisfied, 0 otherwise.

=cut

sub check_one_dep($$) {
	my ($dep, $canditate) = @_;

	my ($pver, $prel) = @{$dep};
	my $ver = $canditate->[0];
	
	return 1 if ($prel eq '*'); # No version constraint
	
	my $cmp = versioncmp($ver, $pver);
	
	if ($prel eq ">>") { return 1 if ($cmp > 0); }
	elsif ($prel eq ">=") { return 1 if ($cmp >= 0); }
	elsif ($prel eq "=") { return 1 if ($cmp == 0); }
	elsif ($prel eq "<=") { return 1 if ($cmp <= 0); }
	elsif ($prel eq "<<") { return 1 if ($cmp < 0); }

	return 0;
}

=item B<CLIP::Pkg::QA::check_depend_atom($pkg, $atom, $hash)>

Internal use only. 
Check that a single depend atom (which can be either a single package or
a logical or of alternatives), represented as 'fuzzy depends' hash reference
$atom, is satisfied by the packages listed in the strict dependency hash 
reference $hash. $pkg is the name of the parent package for which this dependency
is being evaluated, and is passed for logging purposes only.
Return 1 if the dependency is satisfied, 0 otherwise.

=cut

sub check_depend_atom($$$) {
	my ($pkg, $deps, $hash) = @_;

	foreach my $pname (keys %{$deps}) {
		next unless (defined($hash->{$pname}));

		return 1 if (check_one_dep($deps->{$pname}, $hash->{$pname}));
	}

	my $depstring;
	foreach my $pname (keys %{$deps}) {
		$depstring .= $pname;
		$depstring .= "(".$deps->{$pname}->[1]." ".$deps->{$pname}->[0].")"
			unless ($deps->{$pname}->[1] eq '*');
		$depstring .= '|';
	}
	$depstring =~ s/\|$//;
	clip_warn "Dependency $depstring, for package $pkg, is not satisfied";
	return 0;
}

=item B<clippkg_check_depends($pkg, $hash)>

Check that the 'Depends' dependencies of package $pkg (full path) are 
satisfied by the packages referenced in the strict dependency hash ref $hash.
Return 1 if all dependencies are satisfied, 0 otherwise.

=cut

sub clippkg_check_depends($$) {
	my ($pkg, $hash) = @_;
	my $ret = 1;

	my @deplist = ();

	return 0 unless (clippkg_get_fuzzy_deplist($pkg, 'Depends', \@deplist));

	foreach my $dep (@deplist) {
		$ret = 0 unless (check_depend_atom($pkg, $dep, $hash));
	}
	return $ret;
}

=item B<clippkg_check_conflicts($pkg, $hash)>

Check that the 'Conflicts' dependencies of package $pkg (full path) are 
not matched by any of the packages referenced in the strict dependency 
hash ref $hash.
Return 1 if no conflict is found, 0 otherwise.

=cut

sub clippkg_check_conflicts($$) {
	my ($pkg, $hash) = @_;

	my %deps = ();
	my %reps = ();

	return 0 unless (clippkg_get_fuzzy_deps($pkg, 'Conflicts', \%deps));
	return 0 unless (clippkg_get_fuzzy_deps($pkg, 'Replaces', \%reps));
	foreach my $rep (keys %reps) {
		if (defined($deps{$rep})) {
			delete $deps{$rep};
		}
	}

	foreach my $pname (keys %deps) {
		next unless (defined($hash->{$pname}));
		next if ($hash->{$pname}->[1] eq '-'); # don't conflict against provides

		if (check_one_dep($deps{$pname}, $hash->{$pname})) {
			clip_warn "Package $pkg conflicts against $pname (".$deps{$pname}[1]
				." ".$deps{$pname}[0].")";
			return 0;
		}
	}
	return 1;
}

=item B<clippkg_check_deps($pkg, $hash)>

Check the dependencies of package $pkg (full path) against  
the packages referenced in the strict dependency hash ref $hash.
This checks for conflicts and missing 'Depends' dependencies.
Return 1 dependencies are OK, 0 otherwise.

=cut

sub clippkg_check_deps($$) {
	my ($deb, $hash) = @_;
	my $ret = 1;
	$ret = 0 unless (clippkg_check_depends($deb, $hash));
	$ret = 0 unless (clippkg_check_conflicts($deb, $hash));
	return $ret;
}

=back

=cut

1;
__END__

=back

=head1 SEE ALSO

CLIP::Logger(3), dpkg(1)

=head1 AUTHOR

Vincent Strubel, E<lt>clip@ssi.gouv.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011-2013 SGDSN/ANSSI

All rights reserved.

=cut
