pipeline {
  agent any

  stages {

    stage('Checkout') {
      steps {
	      // Get CalibrationResults from GitHub
	      checkout([
          $class: 'GitSCM',
          branches: [[name: 'refs/heads/master']],
          doGenerateSubmoduleConfigurations: false,
          extensions: [[$class: 'RelativeTargetDirectory', relativeTargetDir: 'sykdomspulsen_control']],
          submoduleCfg: [],
          userRemoteConfigs: [[url: 'https://github.com/folkehelseinstituttet/sykdomspulsen_control.git']], poll: true
        ])
      }
    }

    stage('deploy') {
      steps {
        //slackSend(channel: '#jenkins', message: 'Building.')
        sh 'pwd'
        sh 'ls -la'
        // sh '$WORKSPACE/bin/spuls-deploy.sh'
      }
    }
  }
}
