{
	function part1scorer(tree, depth) {
    	return tree
        	.filter(function (e) {
            	return e.type === 'group'
            })
            .map(function (e) {
            	return e.content;
            })
            .reduce(function (score, subtree) {
        		return score + depth + part1scorer(subtree, depth + 1);
        	}, 0);
    }
    
    function part2scorer(tree) {
    	return tree
        	.reduce(function (score, subtree) {
            	if (subtree.type === 'group') {
                	return score + part2scorer(subtree.content);
                } else {
                	return score + subtree.content;
                }
            }, 0);
	}
}

Part1ScoredStream = a:Sequence '\n' {
    return {
    	'Part 1': part1scorer(a, 1),
        'Part 2': part2scorer(a),
        'Parsed Input': a
    };
}

Sequence = a:Thing b:(',' Thing)* {
	return [a].concat(b.map(function (e) { return e[1]; }));
}

Thing = a:(Group / Garbage)

Group = '{' a:Sequence? '}' {
	return {
    	'type': 'group',
        'content': a ? a : []
    }
}

Garbage = '<' a:Stuff '>' {
	return {
    	'type': 'garbage',
        'content': a.join('').length
    }
}

Stuff = (Ignore / Trash)*

Ignore = '!' . { return '' }

Trash = [^>]
