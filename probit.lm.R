probit.pinkseslade <- function(W,u) {   # u is standardized gen residual
# u is residual3 from probit.predict
	trww <- sum( W * t(W) )
	trwpw <- sum( W^2 )
	tt <- trww + trwpw
    Wu <- W %*% u
	uWu <- crossprod(u,Wu)
	lmps <- (uWu^2) / tt
    plmps <- pchisq(lmps,1,lower.tail=FALSE)
    result <- list(lm=lmps,plm=plmps)
    result
	}
	
probit.pinkse <- function(W,u,varcox) {   # u is Cox-Snell, varcox its var
# u is residual2  from probit.predict
# varcox is varcox from probit.predict
	trww <- sum( W * t(W) )
	trwpw <- sum( W^2 )
	tt <- trww + trwpw
    Wu <- W %*% u
	uWu <- crossprod(u,Wu)
	lmps <- (uWu^2) / (varcox * tt)
    plmps <- pchisq(lmps,1,lower.tail=FALSE)
    result <- list(lm=lmps,plm=plmps)
    result
	}
	
probit.kp <- function(W,u,v)   {   # u is residual, v its variance vector
# u is residual1 from probit.predict
# v is vresidual1 from probit.predict
	sigw <- v[,1] * W
	sigwp <- v[,1] * t(W)
	t1 <- sum (sigw * t(sigw) )
	t2 <- sum (sigw * t(sigwp) )
	tt <- t1 + t2
    Wu <- W %*% u
	uWu <- crossprod(u,Wu)
	lmps <- (uWu^2) / tt
    plmps <- pchisq(lmps,1,lower.tail=FALSE)
    result <- list(lm=lmps,plm=plmps)
    result	
	}