real expgamma_lpdf(vector x, real shape_k, real scale_theta){

    return(sum(
	-shape_k * log(scale_theta) -
        lgamma(shape_k) +
        shape_k * x - (exp(x) / scale_theta)));
}

real expgamma_lpdf(real x, real shape_k, real scale_theta){

    return(
	-shape_k * log(scale_theta) -
        lgamma(shape_k) +
        shape_k * x - (exp(x) / scale_theta));
}

real expgamma_lpdf(vector xs,
                   vector shapes_k,
		   vector scales_theta){

    return(sum(
	-shapes_k .* log(scales_theta) -
        lgamma(shapes_k) +
        shapes_k .* xs - (exp(xs) ./ scales_theta)));
}
