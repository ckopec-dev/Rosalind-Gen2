# Rosalind Problem: Implement Viterbi Learning

I'll solve the Viterbi Learning problem in Perl. This problem involves learning the transition and emission probabilities of a Hidden Markov Model (HMM) from observed sequences using the Baum-Welch algorithm.

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the input
my $observations = $lines[0];
my $num_observations = length($observations);
my @obs = split //, $observations;

my @states = split /\s+/, $lines[2];
my $num_states = @states;

# Parse transition matrix
my @transitions = split /\s+/, $lines[4];
my @transition_matrix;
for my $i (0..$num_states-1) {
    for my $j (0..$num_states-1) {
        $transition_matrix[$i][$j] = $transitions[$i * $num_states + $j];
    }
}

# Parse emission matrix
my @emissions = split /\s+/, $lines[6];
my @emission_matrix;
for my $i (0..$num_states-1) {
    for my $j (0..$num_states-1) {
        $emission_matrix[$i][$j] = $emissions[$i * $num_states + $j];
    }
}

# Initialize probabilities
my @pi = (1.0 / $num_states) x $num_states;

# Viterbi Learning Algorithm
sub viterbi_learning {
    my ($observations, $states, $pi, $transition_matrix, $emission_matrix, $num_iterations) = @_;
    
    my @current_pi = @$pi;
    my @current_transitions = map { [@$_] } @$transition_matrix;
    my @current_emissions = map { [@$_] } @$emission_matrix;
    
    # Set number of iterations
    my $iterations = $num_iterations || 100;
    
    for my $iter (1..$iterations) {
        # E-step: compute forward and backward probabilities
        my ($alpha, $beta) = compute_forward_backward($observations, $states, 
                                                     \@current_pi, \@current_transitions, 
                                                     \@current_emissions);
        
        # M-step: recompute parameters
        my ($new_pi, $new_transitions, $new_emissions) = 
            recompute_parameters($observations, $states, $alpha, $beta, 
                               \@current_transitions, \@current_emissions);
        
        # Update parameters
        @current_pi = @$new_pi;
        @current_transitions = map { [@$_] } @$new_transitions;
        @current_emissions = map { [@$_] } @$new_emissions;
    }
    
    return (\@current_pi, \@current_transitions, \@current_emissions);
}

# Forward-backward algorithm
sub compute_forward_backward {
    my ($observations, $states, $pi, $transition_matrix, $emission_matrix) = @_;
    
    my $n = scalar @$observations;
    my $m = scalar @$states;
    
    # Forward probabilities
    my @alpha;
    for my $i (0..$m-1) {
        $alpha[0][$i] = $pi->[$i] * $emission_matrix->[$i][ord($observations->[0]) - ord('A')];
    }
    
    for my $t (1..$n-1) {
        for my $j (0..$m-1) {
            my $sum = 0;
            for my $i (0..$m-1) {
                $sum += $alpha[$t-1][$i] * $transition_matrix->[$i][$j];
            }
            $alpha[$t][$j] = $sum * $emission_matrix->[$j][ord($observations->[$t]) - ord('A')];
        }
    }
    
    # Backward probabilities
    my @beta;
    for my $i (0..$m-1) {
        $beta[$n-1][$i] = 1;
    }
    
    for my $t (reverse 0..$n-2) {
        for my $i (0..$m-1) {
            my $sum = 0;
            for my $j (0..$m-1) {
                $sum += $transition_matrix->[$i][$j] * $emission_matrix->[$j][ord($observations->[$t+1]) - ord('A')] * $beta[$t+1][$j];
            }
            $beta[$t][$i] = $sum;
        }
    }
    
    return (\@alpha, \@beta);
}

# Recompute parameters
sub recompute_parameters {
    my ($observations, $states, $alpha, $beta, $transition_matrix, $emission_matrix) = @_;
    
    my $n = scalar @$observations;
    my $m = scalar @$states;
    
    # Recompute initial probabilities
    my @new_pi = (0) x $m;
    for my $i (0..$m-1) {
        $new_pi[$i] = $alpha->[0][$i] * $beta->[0][$i];
    }
    
    # Normalize pi
    my $pi_sum = sum(@new_pi);
    for my $i (0..$m-1) {
        $new_pi[$i] /= $pi_sum;
    }
    
    # Recompute transition probabilities
    my @new_transitions = map { [0] x $m } 0..$m-1;
    for my $i (0..$m-1) {
        for my $j (0..$m-1) {
            my $numerator = 0;
            my $denominator = 0;
            
            for my $t (0..$n-2) {
                $numerator += $alpha->[$t][$i] * $transition_matrix->[$i][$j] * 
                             $emission_matrix->[$j][ord($observations->[$t+1]) - ord('A')] * 
                             $beta->[$t+1][$j];
                $denominator += $alpha->[$t][$i] * $beta->[$t][$i];
            }
            
            $new_transitions[$i][$j] = $denominator > 0 ? $numerator / $denominator : 0;
        }
    }
    
    # Recompute emission probabilities
    my @new_emissions = map { [0] x $m } 0..$m-1;
    for my $i (0..$m-1) {
        for my $k (0..$m-1) {
            my $numerator = 0;
            my $denominator = 0;
            
            for my $t (0..$n-1) {
                if (ord($observations->[$t]) - ord('A') == $k) {
                    $numerator += $alpha->[$t][$i] * $beta->[$t][$i];
                }
                $denominator += $alpha->[$t][$i] * $beta->[$t][$i];
            }
            
            $new_emissions[$i][$k] = $denominator > 0 ? $numerator / $denominator : 0;
        }
    }
    
    return (\@new_pi, \@new_transitions, \@new_emissions);
}

# Run Viterbi Learning
my ($new_pi, $new_transitions, $new_emissions) = 
    viterbi_learning(\@obs, \@states, \@pi, \@transition_matrix, \@emission_matrix, 100);

# Print results
print "Initial probabilities:\n";
for my $i (0..$#states) {
    printf "%.3f\n", $new_pi->[$i];
}

print "Transition matrix:\n";
for my $i (0..$#states) {
    for my $j (0..$#states) {
        printf "%.3f ", $new_transitions[$i][$j];
    }
    print "\n";
}

print "Emission matrix:\n";
for my $i (0..$#states) {
    for my $j (0..$#states) {
        printf "%.3f ", $new_emissions[$i][$j];
    }
    print "\n";
}
```

This solution implements the Viterbi Learning algorithm for HMM parameter estimation:

1. **Input Parsing**: Reads the observation sequence, states, and initial transition/emission matrices
2. **Initialization**: Sets up initial probabilities for the HMM
3. **Viterbi Learning**: Iteratively applies the Baum-Welch algorithm:
   - E-step: Computes forward and backward probabilities using the current parameters
   - M-step: Recomputes the transition and emission probabilities based on the computed probabilities
4. **Output**: Prints the learned parameters

The algorithm converges to a local optimum of the likelihood function by alternating between the E-step (computing expected sufficient statistics) and M-step (maximizing the likelihood with respect to the model parameters).

