{
	function part1scorer(tree, depth) {
    	return tree.reduce(function (score, subtree) {
        	return score + depth + part1scorer(subtree, depth + 1);
        }, 0);
    }
}

Part1ScoredStream = a:Sequence '\n' {   
    return part1scorer(a, 1);
}

Sequence = a:Thing b:(',' Thing)* {
	return [a].concat(b.map(function (e) { return e[1]; }))
        .filter(function (e) { return e.type === 'group'; })
      	.map(function (e) { return e.contents; });
}

Thing = a:(Group / Garbage)

Group = '{' a:Sequence? '}' {
	return {
    	'type': 'group',
        'contents': a ? a : []
    }
}

Garbage = '<' Stuff '>' {
	return {
    	'type': 'garbage'
    }
}

Stuff = (Ignore / Trash)*

Ignore = '!' .

Trash = [^>]
