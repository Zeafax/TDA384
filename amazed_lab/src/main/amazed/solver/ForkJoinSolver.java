package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */

public class ForkJoinSolver
        extends SequentialSolver {

    private int player;
    private int steps_taken;
    private static final AtomicBoolean goalFound = new AtomicBoolean(false);

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
        this.visited = new ConcurrentSkipListSet<>();
        this.steps_taken = 0;
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     *         goal node in the maze; <code>null</code> if such a path cannot
     *         be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        // List of tasks to fork
        List<ForkJoinSolver> tasks = new ArrayList<>();

        // Initialize first thread
        if (frontier.isEmpty()) {
            frontier = new Stack<>();
            predecessor = new HashMap<>();
            frontier.push(start);
            player = maze.newPlayer(start);
        }

        while (!frontier.isEmpty()) {
            // Check if goal has been found by another thread
            if (goalFound.get()) {
                break;
            }

            int current = frontier.pop();

            // Check if current node is a goal
            if (maze.hasGoal(current)) {
                maze.move(player, current);
                goalFound.set(true);
                return pathFromTo(start, current);
            }

            // If current node has not been visited
            if (!visited.contains(current)) {
                maze.move(player, current);
                steps_taken++;
                visited.add(current);

                // Get neighbors of current node
                Set<Integer> neighbors = maze.neighbors(current);
                // Filter out visited neighbors
                List<Integer> unvisitedNeighbors = new ArrayList<>();
                for (int neighbor : neighbors) {
                    if (!visited.contains(neighbor)) {
                        unvisitedNeighbors.add(neighbor);
                    }
                }

                // If there are more than one unvisited neighbor, fork new tasks
                if (unvisitedNeighbors.size() > 1) {
                    for (int neighbor : unvisitedNeighbors) {
                        predecessor.put(neighbor, current);
                        if (steps_taken >= forkAfter) {
                            steps_taken = 0;
                            // Fork new task and add necessary data
                            ForkJoinSolver task = new ForkJoinSolver(maze, forkAfter);
                            task.visited = this.visited;
                            task.predecessor = new HashMap<>(predecessor);
                            task.frontier = new Stack<>();
                            task.frontier.push(neighbor);
                            task.player = maze.newPlayer(neighbor);
                            tasks.add(task);
                            task.fork();
                        } else {
                            frontier.push(neighbor);
                        }
                    }
                } else {
                    // If there is only one unvisited neighbor, add it to the frontier
                    for (int neighbor : unvisitedNeighbors) {
                        predecessor.put(neighbor, current);
                        frontier.push(neighbor);
                    }
                }
            }
        }

        // Join all tasks and return result if goal has been found
        for (ForkJoinSolver task : tasks) {
            List<Integer> result = task.join();
            if (result != null) {
                return result;
            }
        }

        return null;
    }
}
