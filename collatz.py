import matplotlib.pyplot as plt
import random
import argparse
import numpy as np
import sys

def collatz_next(number):
    """Calculate the next number in the Collatz sequence."""
    if number % 2 == 0:             # if number is even
        next_val = number // 2      # divide in half
    else:                           # if number is odd
        next_val = 3 * number + 1   # times 3 plus 1
    return next_val                 # return value

def collatz_sequence(start_num):
    """Generate the complete Collatz sequence for a starting number."""
    sequence = [start_num]
    while start_num != 1:
        start_num = collatz_next(start_num)
        sequence.append(start_num)
    return sequence

def plot_collatz(sequence):
    """Plot the Collatz sequence on an x-y graph."""
    plt.figure(figsize=(10, 6))
    plt.plot(range(len(sequence)), sequence, marker='o', linestyle='-')
    plt.title('Collatz Sequence for Starting Number ' + str(sequence[0]))
    plt.xlabel('Step Number')
    plt.ylabel('Value')
    plt.grid(True)
    plt.savefig('collatz_plot.png')  # Save the plot as an image
    print(f"Plot saved as 'collatz_plot.png'")
    try:
        plt.show()  # Display the plot
    except:
        pass

def create_legend(plot_lines, branches_to_plot):
    """Create a legend for the plot with correct colors."""
    if len(branches_to_plot) <= 20:
        plt.legend([line for line, _ in plot_lines], 
                   [label for _, label in plot_lines],
                   loc='center left', bbox_to_anchor=(1, 0.5))
    else:
        # Limit to first 20 branches for legend
        plt.legend([line for line, _ in plot_lines[:20]], 
                   [label for _, label in plot_lines[:20]],
                   loc='center left', bbox_to_anchor=(1, 0.5),
                   title=f'Showing 20 of {len(branches_to_plot)} branches')

def setup_plot_layout(max_length):
    """Set up the plot layout and labels."""
    plt.title('Collatz Branches Tree Visualization (Converging at End)')
    plt.xlabel('Step Number')
    plt.ylabel('Value')
    plt.grid(True, alpha=0.3)
    plt.xlim(0, max_length)
    plt.tight_layout()

def save_and_show_plot(filename):
    """Save the plot to a file and attempt to display it."""
    plt.savefig(filename, bbox_inches='tight')
    print(f"Plot saved as '{filename}'")
    try:
        plt.show()
    except:
        pass

def plot_collatz_branches(branches, max_branches=None):
    """
    Plot multiple Collatz branches on the same graph, converging at the end.
    
    Parameters:
    branches -- list of Collatz sequences to plot
    max_branches -- maximum number of branches to plot (None for all)
    """
    # Limit the number of branches if specified
    if max_branches is not None and max_branches < len(branches):
        branches_to_plot = branches[:max_branches]
        print(f"Plotting {max_branches} of {len(branches)} branches")
    else:
        branches_to_plot = branches
    
    plt.figure(figsize=(14, 8))  # Wider figure to accommodate legend on the side
    
    # Create a color map with distinct colors for each branch
    colors = plt.cm.viridis(np.linspace(0, 1, len(branches_to_plot)))
    
    # Find the maximum length to determine the x-axis range
    max_length = max(len(branch) for branch in branches_to_plot)
    
    # Store plot lines for the legend
    plot_lines = []
    
    # Plot each branch with a different color, aligned at the end
    for i, branch in enumerate(branches_to_plot):
        # Calculate padding to align at the end
        padding = max_length - len(branch)
        
        # Create x-coordinates that align the end points
        x_coords = np.arange(padding, max_length)
        
        # Store the line object for legend
        line, = plt.plot(
            x_coords, 
            branch, 
            marker='.', 
            linestyle='-', 
            linewidth=1.5,
            alpha=0.7,
            color=colors[i],  # Use the specific color from the colormap
        )
        
        # Add to our list of lines with proper label
        plot_lines.append((line, f'Start: {branch[0]}'))
    
    # Create legend, set up layout, and show plot
    create_legend(plot_lines, branches_to_plot)
    setup_plot_layout(max_length)
    save_and_show_plot('collatz_branches_converging.png')

def collatz_branches(start_num):
    """Calculate all unique Collatz branches starting from a given number."""
    master_set = set()
    branches = []
    while start_num != 1:
        if start_num in master_set:
            start_num -= 1
        else:
            branches.append(collatz_sequence(start_num))
            master_set.update(branches[-1])
            start_num -= 1
    return branches

def get_starting_number():
    """Get a starting number from user input."""
    while True:
        try:
            response = input("Please enter a number, or r for random: ")
            if response == 'r':
                start_num = random.randint(1, 1000)
                print(f"Random starting number: {start_num}")
                break
            elif int(response) <= 0:
                print("Invalid input. Please enter a positive integer.")
            else:
                start_num = int(response)
                break
        except ValueError:
            print("Invalid input.")
    return start_num

def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description='Collatz Conjecture Calculator and Visualizer')
    
    # Starting number options (mutually exclusive)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-n', '--number', type=int, help='Starting number for the Collatz sequence')
    group.add_argument('-r', '--random', action='store_true', help='Use a random starting number')
    
    # Mode and visualization options
    parser.add_argument('-b', '--branches', action='store_true', help='Show branches of the sequence')
    parser.add_argument('-g', '--graph', action='store_true', help='Show graph of the sequence')
    parser.add_argument('-m', '--max', type=int, help='Maximum number of branches to plot')
    parser.add_argument('-s', '--sequence', action='store_true', help='Display the full sequence in single mode')
    
    return parser.parse_args()

def get_user_preferences(args):
    """Get user preferences from args or interactive prompts."""
    preferences = {
        'starting_number': None,
        'show_branches': False,
        'show_graph': False,
        'max_branches': None,
        'show_sequence': False
    }
    
    # Get starting number
    if args.number:
        preferences['starting_number'] = args.number
    elif args.random:
        preferences['starting_number'] = random.randint(1, 1000)
        print(f"Random starting number: {preferences['starting_number']}")
    else:
        preferences['starting_number'] = get_starting_number()
    
    # Determine if showing branches
    if args.branches:
        preferences['show_branches'] = True
    elif not any([args.number, args.random, args.graph, args.max, args.sequence]):  # No CLI args provided
        response = input("Would you like to see the branches of the sequence? (y/n): ")
        preferences['show_branches'] = response.lower() == 'y'
    
    # Determine if showing graph
    if args.graph:
        preferences['show_graph'] = True
    elif not any([args.number, args.random, args.branches, args.max, args.sequence]):  # No CLI args provided
        if preferences['show_branches']:
            response = input("Would you like to see a graph of all branches? (y/n): ")
        else:
            response = input("Would you like to see a graph of the sequence? (y/n): ")
        preferences['show_graph'] = response.lower() == 'y'
    
    # Get max branches if needed
    if preferences['show_branches'] and preferences['show_graph']:
        if args.max is not None:
            preferences['max_branches'] = args.max
        elif not any([args.number, args.random, args.branches, args.graph, args.sequence]):  # No CLI args provided
            limit_input = input("How many branches to plot? (Enter for all): ")
            if limit_input.strip():
                try:
                    preferences['max_branches'] = int(limit_input)
                    if preferences['max_branches'] <= 0:
                        preferences['max_branches'] = None
                except ValueError:
                    print("Invalid input. Using all branches")
    
    # Set sequence display preference (CLI only)
    preferences['show_sequence'] = args.sequence
    
    return preferences

def main():
    """Main function to run the Collatz program."""
    args = parse_arguments()
    prefs = get_user_preferences(args)
    
    # Calculate sequence or branches
    if prefs['show_branches']:
        branches = collatz_branches(prefs['starting_number'])
        print(f"Found {len(branches)} unique branches")
        
        if prefs['show_graph']:
            plot_collatz_branches(branches, prefs['max_branches'])
    else:
        seq = collatz_sequence(prefs['starting_number'])
        print(f"Collatz of {prefs['starting_number']} took {len(seq)} steps")
        
        # Display the full sequence if requested
        if prefs['show_sequence']:
            print("Full sequence:")
            print(seq)
        
        if prefs['show_graph']:
            plot_collatz(seq)

if __name__ == "__main__":
    main()
