function getFaunaClientKey() {
  return process.env.FAUNADB_CLIENT_SECRET;
}

function isValidUser(user) {
  return user && user.exp > (new Date).getTime() / 1000;
}


exports.handler = async function(event, context) {
  const { user } = context.clientContext;
  if (isValidUser(user)) {
    console.log('valid user', user);
    return {
      statusCode: 200,
      body: JSON.stringify({ faunaKey: getFaunaClientKey() }) 
    };
  } 

  console.log('invalid user');
  return {
    statusCode: 401,
    body: JSON.stringify({ message: 'User token invalid or not present' })
  };
}

