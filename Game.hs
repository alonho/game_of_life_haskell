import Higher
import Control.Concurrent
import System.Random

over_populated n = n > 3
under_populated n = n < 2
live_stays_alive = notP $ orP [over_populated, under_populated]
dead_revives n = n == 3
next_state True = live_stays_alive
next_state False = dead_revives

get_legal_nbr_range a i = [max (i - 1) 0..min (i + 1) (length a - 1)]
get_nbr_indices a x y = [(ix, iy) | ix <- get_legal_nbr_range a x,
		      	      	    iy <- get_legal_nbr_range (a !! ix) y, 
			 ix /= x || iy /= y]
get_nbrs a x y =  map (\(x, y) -> a !! x !! y) (get_nbr_indices a x y)
get_num_nbrs a x y = sum . map fromEnum $ get_nbrs a x y

cell_repr True = '.'
cell_repr False = ' '
line_repr a = (map cell_repr a) ++ "\n"
board_repr a = concat $ map line_repr a

next_cell_state a x y = next_state (a !! x !! y) (get_num_nbrs a x y)
next_board a = map (\x -> map (\y -> next_cell_state a x y) 
	                      [0..(length $ a !! x) - 1]) 
	           [0..length a - 1]

_main board = do putStr $ board_repr board
                 threadDelay 1000
           	 _main (next_board board)

bools n r = take n (randomRs (False, True) (mkStdGen r))

main :: IO ()
main = _main [bools 50 r | r <- [1..25]]
