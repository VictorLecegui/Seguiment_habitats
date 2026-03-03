

p_malla = hic_points
n_points = 30

points_rand_legacy <- function(p_malla){

    rand_points <- irs(p_malla, n_base = n_target, 
                                n_over = n_reserva,
                                legacy_sites = legacy_points,
                                mindis = min_dist,
                                maxtry = 100 )
    points_bined <- sp_rbind(rand_points)
    bal <- sp_balance(points_bined, p_malla)
    return(bal$value)

}

points_rand_no_legacy <- function(p_malla){

    rand_points <- irs(p_malla, n_base = n_target, 
                                n_over = n_reserva,
                                mindis = min_dist,
                                maxtry = 100 )
                                
    points_bined <- sp_rbind(rand_points)
    bal <- sp_balance(points_bined, p_malla)
    return(bal$value)
}


n_iter = 999

rand_balances <- replicate(
  n_iter,
  points_rand_legacy(p_malla = hic_points,
                    n_points = n_target)
)


grts_balances <- replicate(n_iter, 
        points_grts_bal(p_malla = hic_points, n_points = n_target))



paste(mean(rand_balances), "+-", sd(rand_balances))
paste(mean(grts_balances), "+-", sd(grts_balances))

sd(grts_balances)

p_value <- (sum(rand_balances <= points_bal$value) + 1) / (n_iter + 1)

mean(rand_balances)
points_bal$value
hist(rand_balances, breaks = 30,
     main = "Random spatial balance distribution",
     xlab = "sp_balance")

abline(v = points_bal, lwd = 3)
